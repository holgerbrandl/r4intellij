package com.r4intellij.debugger;

import com.r4intellij.debugger.exception.RDebuggerException;
import com.r4intellij.debugger.executor.RExecutionResult;
import com.r4intellij.debugger.executor.RExecutor;
import org.jetbrains.annotations.NotNull;
import org.mockito.InOrder;
import org.mockito.Mockito;

import java.util.List;
import java.util.Map;

import static org.mockito.Mockito.inOrder;
import static org.mockito.Mockito.when;

public class MockitoUtils {

    @NotNull
    public static RExecutor setupExecutor(@NotNull final Map<String, List<RExecutionResult>> commandsAndResults)
            throws RDebuggerException {
        final RExecutor result = Mockito.mock(RExecutor.class);

        for (Map.Entry<String, List<RExecutionResult>> commandAndResult : commandsAndResults.entrySet()) {
            final List<RExecutionResult> value = commandAndResult.getValue();

            if (value.size() == 1) {
                when(result.execute(commandAndResult.getKey())).thenReturn(value.get(0));
            } else {
                final RExecutionResult firstResult = value.get(0);
                final RExecutionResult[] otherResults = value.subList(1, value.size()).toArray(new RExecutionResult[value.size() - 1]);

                when(result.execute(commandAndResult.getKey())).thenReturn(firstResult, otherResults);
            }
        }

        return result;
    }


    public static void verifyExecutor(@NotNull final RExecutor executor, @NotNull final List<String> commands)
            throws RDebuggerException {
        final InOrder inOrder = inOrder(executor);

        for (final String command : commands) {
            inOrder.verify(executor).execute(command);
        }
    }
}
