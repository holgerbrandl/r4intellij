package com.r4intellij.debugger.function;

import com.intellij.openapi.util.TextRange;
import com.intellij.util.containers.hash.HashMap;
import com.r4intellij.debugger.MockitoUtils;
import com.r4intellij.debugger.ROutputReceiver;
import com.r4intellij.debugger.exception.RDebuggerException;
import com.r4intellij.debugger.executor.RExecutionResult;
import com.r4intellij.debugger.executor.RExecutionResultType;
import com.r4intellij.debugger.executor.RExecutor;
import com.r4intellij.debugger.mock.MockROutputReceiver;
import org.jetbrains.annotations.NotNull;
import org.junit.Test;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Map;

import static com.r4intellij.debugger.data.RCommands.*;
import static com.r4intellij.debugger.data.RFunctionConstants.*;
import static com.r4intellij.debugger.data.RLanguageConstants.CLOSURE;
import static com.r4intellij.debugger.data.RLanguageConstants.FUNCTION_TYPE;
import static com.r4intellij.debugger.function.RTraceAndDebugUtils.traceAndDebugFunctions;
import static com.r4intellij.debugger.mock.MockRExecutor.LS_FUNCTIONS_ERROR;
import static org.junit.Assert.assertEquals;
import static org.mockito.Mockito.*;

public class RTraceAndDebugUtilsTest {

    @NotNull
    public static final String LS_FUNCTIONS_COMMAND = filterCommand(
            "function(x) x == \"" + CLOSURE + "\"",
            eapplyCommand(ENVIRONMENT_COMMAND, TYPEOF_FUNCTION)
    );

    @NotNull
    public static final RExecutionResult NO_FUNCTIONS_RESULT = new RExecutionResult(
            "named list()",
            RExecutionResultType.RESPONSE,
            TextRange.allOf("named list()"),
            LS_FUNCTIONS_ERROR
    );


    @Test
    public void empty() throws RDebuggerException {
        final RExecutor executor = mock(RExecutor.class);
        final ROutputReceiver receiver = mock(ROutputReceiver.class);

        when(executor.execute(LS_FUNCTIONS_COMMAND)).thenReturn(NO_FUNCTIONS_RESULT);

        traceAndDebugFunctions(executor, receiver);

        verify(executor, times(1)).execute(LS_FUNCTIONS_COMMAND);
        verify(receiver, times(1)).receiveError(LS_FUNCTIONS_ERROR);

        verifyNoMoreInteractions(executor);
        verifyNoMoreInteractions(receiver);
    }


    @Test
    public void ordinary() throws RDebuggerException {
        final String xFunctionName = "x";
        final String mainFunctionName = MAIN_FUNCTION_NAME;
        final String deviceFunctionName = SERVICE_FUNCTION_PREFIX + "device_init";
        final String xEnterFunctionName = SERVICE_FUNCTION_PREFIX + xFunctionName + SERVICE_ENTER_FUNCTION_SUFFIX;
        final String mainEnterFunctionName = SERVICE_FUNCTION_PREFIX + mainFunctionName + SERVICE_ENTER_FUNCTION_SUFFIX;

        final List<String> commands = getCommands(xFunctionName, mainFunctionName, xEnterFunctionName, mainEnterFunctionName);
        final List<RExecutionResult> results = getResults(xFunctionName, mainFunctionName, deviceFunctionName, xEnterFunctionName);
        final Map<String, List<RExecutionResult>> commandsAndResults = getCommandsAndResults(commands, results);

        final List<String> errors = Arrays.asList(
                LS_FUNCTIONS_ERROR,
                "error_" + xFunctionName + "_e",
                "error_" + xFunctionName + "_d",
                "error_" + mainFunctionName + "_e",
                "error_" + mainFunctionName + "_d"
        );

        final RExecutor executor = MockitoUtils.setupExecutor(commandsAndResults);
        final MockROutputReceiver receiver = new MockROutputReceiver();

        traceAndDebugFunctions(executor, receiver);

        MockitoUtils.verifyExecutor(executor, commands);
        assertEquals(Collections.emptyList(), receiver.getOutputs());
        assertEquals(errors, receiver.getErrors());

        verifyNoMoreInteractions(executor);
    }


    @NotNull
    private Map<String, List<RExecutionResult>> getCommandsAndResults(@NotNull final List<String> commands,
                                                                      @NotNull final List<RExecutionResult> results) {
        final Map<String, List<RExecutionResult>> commandsAndResults = new HashMap<String, List<RExecutionResult>>();

        for (int i = 0; i < commands.size(); i++) {
            commandsAndResults.put(commands.get(i), Collections.singletonList(results.get(i)));
        }

        return commandsAndResults;
    }


    @NotNull
    private List<String> getCommands(@NotNull final String xFunctionName,
                                     @NotNull final String mainFunctionName,
                                     @NotNull final String xEnterFunctionName,
                                     @NotNull final String mainEnterFunctionName) {
        return Arrays.asList(
                LS_FUNCTIONS_COMMAND,

                xEnterFunctionName + " <- function() { print(\"" + xFunctionName + "\") }",
                traceCommand(xFunctionName, xEnterFunctionName),
                debugCommand(xFunctionName),

                mainEnterFunctionName + " <- function() { print(\"" + mainFunctionName + "\") }",
                traceCommand(mainFunctionName, mainEnterFunctionName),
                debugCommand(mainFunctionName)
        );
    }


    @NotNull
    private List<RExecutionResult> getResults(@NotNull final String xFunctionName,
                                              @NotNull final String mainFunctionName,
                                              @NotNull final String deviceFunctionName,
                                              @NotNull final String xEnterFunctionName) {
        final String lsFunctionsOutput = "$" + xFunctionName + "\n" +
                FUNCTION_TYPE + "\n\n" +
                "$" + xEnterFunctionName + "\n" +
                FUNCTION_TYPE + "\n\n" +
                "$" + deviceFunctionName + "\n" +
                FUNCTION_TYPE + "\n\n" +
                "$" + mainFunctionName + "\n" +
                FUNCTION_TYPE;

        return Arrays.asList(
                new RExecutionResult(
                        lsFunctionsOutput,
                        RExecutionResultType.RESPONSE,
                        TextRange.allOf(lsFunctionsOutput),
                        LS_FUNCTIONS_ERROR
                ),
                new RExecutionResult(
                        "",
                        RExecutionResultType.EMPTY,
                        TextRange.EMPTY_RANGE,
                        "error_" + xFunctionName + "_e"
                ),
                new RExecutionResult(
                        "[1] \"" + xFunctionName + "\"",
                        RExecutionResultType.RESPONSE,
                        TextRange.allOf("[1] \"" + xFunctionName + "\""),
                        "error_" + xFunctionName + "_t"
                ),
                new RExecutionResult(
                        "",
                        RExecutionResultType.EMPTY,
                        TextRange.EMPTY_RANGE,
                        "error_" + xFunctionName + "_d"
                ),
                new RExecutionResult(
                        "",
                        RExecutionResultType.EMPTY,
                        TextRange.EMPTY_RANGE,
                        "error_" + mainFunctionName + "_e"
                ),
                new RExecutionResult(
                        "[1] \"" + mainFunctionName + "\"",
                        RExecutionResultType.RESPONSE,
                        TextRange.allOf("[1] \"" + mainFunctionName + "\""),
                        "error_" + mainFunctionName + "_t"
                ),
                new RExecutionResult(
                        "",
                        RExecutionResultType.EMPTY,
                        TextRange.EMPTY_RANGE,
                        "error_" + mainFunctionName + "_d"
                )
        );
    }
}