package com.r4intellij.run;

import com.intellij.openapi.project.Project;
import com.r4intellij.debugger.RDebuggerStringUtils;
import com.r4intellij.debugger.data.RCommands;
import com.r4intellij.debugger.exception.RDebuggerException;
import com.r4intellij.debugger.executor.RExecutionResultType;
import com.r4intellij.debugger.executor.RExecutor;
import com.r4intellij.debugger.executor.RExecutorUtils;
import com.r4intellij.run.graphics.RGraphicsUtils;
import org.jetbrains.annotations.NotNull;

// TODO [run][test]
public final class RProcessUtils {

    public static void executeInitGraphicsCommands(@NotNull final Project project, @NotNull final RExecutor executor)
            throws RDebuggerException {
//        if(true) return;

        final boolean is64Bit = is64Bit(loadArchitecture(executor));

        for (final String command : RGraphicsUtils.calculateInitCommands(project, is64Bit)) {
            executor.execute(command);
        }
    }


    private static boolean is64Bit(@NotNull final String architecture) {
        final int begin = RDebuggerStringUtils.findNextLineBegin(architecture, 0) + 5;
        final int end = RDebuggerStringUtils.findCurrentLineEnd(architecture, begin) - 1;

        return begin <= end && architecture.substring(begin, end).equals("x86_64");
    }


    @NotNull
    private static String loadArchitecture(@NotNull final RExecutor executor) throws RDebuggerException {
        return RExecutorUtils.execute(
                executor,
                RCommands.rVersionCommand("arch"),
                RExecutionResultType.RESPONSE
        ).getOutput();
    }
}
