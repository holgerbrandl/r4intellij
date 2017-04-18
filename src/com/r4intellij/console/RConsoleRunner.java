package com.r4intellij.console;

import com.intellij.execution.ExecutionException;
import com.intellij.execution.configurations.GeneralCommandLine;
import com.intellij.execution.console.LanguageConsoleImpl;
import com.intellij.execution.console.LanguageConsoleView;
import com.intellij.execution.console.ProcessBackedConsoleExecuteActionHandler;
import com.intellij.execution.process.ColoredProcessHandler;
import com.intellij.execution.process.OSProcessHandler;
import com.intellij.execution.process.ProcessOutputTypes;
import com.intellij.execution.runners.AbstractConsoleRunnerWithHistory;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.Key;
import com.intellij.openapi.util.SystemInfo;
import com.intellij.openapi.util.text.StringUtil;
import com.r4intellij.RFileType;
import com.r4intellij.RLanguage;
import com.r4intellij.debugger.data.RInterpreterConstants;
import com.r4intellij.settings.RSettings;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import javax.swing.*;
import java.util.Objects;

public class RConsoleRunner extends AbstractConsoleRunnerWithHistory<LanguageConsoleView> {

    public RConsoleRunner(@NotNull final Project project, @Nullable final String workingDir) {
        super(project, "R Console", workingDir);
    }


    @Nullable
    @Override
    protected Icon getConsoleIcon() {
        return RFileType.INSTANCE.getIcon();
    }


    @NotNull
    @Override
    protected LanguageConsoleView createConsoleView() {
        final LanguageConsoleImpl console = new LanguageConsoleImpl(getProject(), getConsoleTitle(), RLanguage.getInstance());
//        console.setPrompt(null);
        return console;
    }


    @NotNull
    @Override
    protected Process createProcess() throws ExecutionException {
        return getCommandLine(getInterpreterPath()).createProcess();
    }


    boolean hasPendingPrompt = false;


    @NotNull
    @Override
    protected OSProcessHandler createProcessHandler(@NotNull final Process process) {
        final String commandLine = getCommandLine(RSettings.getInstance().getInterpreterPath()).getCommandLineString();
        return new ColoredProcessHandler(process, commandLine) {

            @Override
            public void coloredTextAvailable(@NotNull String s, @NotNull Key key) {
                if (Objects.equals(ProcessOutputTypes.STDOUT, key)) {
                    if (s.trim().equals(">")) {
                        hasPendingPrompt = true;
                        return;
                    }
                    if (hasPendingPrompt) {
                        s = "> " + s;
                        hasPendingPrompt = false;
                    }
                }

                super.coloredTextAvailable(s, key);
            }
        };
    }


    @NotNull
    @Override
    protected ProcessBackedConsoleExecuteActionHandler createExecuteActionHandler() {
        final ProcessBackedConsoleExecuteActionHandler handler = new ProcessBackedConsoleExecuteActionHandler(getProcessHandler(), false);
        handler.setAddCurrentToHistory(false);
        return handler;
    }


    @NotNull
    private GeneralCommandLine getCommandLine(@NotNull final String exePath) {
        return new GeneralCommandLine()
                .withExePath(exePath)
                .withParameters(RInterpreterConstants.QUIET_PARAMETER, SystemInfo.isWindows ? "--ess" : "--interactive")
                .withWorkDirectory(getWorkingDir())
                .withParentEnvironmentType(GeneralCommandLine.ParentEnvironmentType.CONSOLE);
    }


    @NotNull
    private String getInterpreterPath() throws ExecutionException {
        final String interpreterPath = RSettings.getInstance().getInterpreterPath();

        if (StringUtil.isEmptyOrSpaces(interpreterPath)) {
            throw new ExecutionException("R interpreter is not specified");
        }

        return interpreterPath;
    }
}
