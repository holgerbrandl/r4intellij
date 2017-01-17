package com.r4intellij.packages;

import com.google.common.collect.Lists;
import com.intellij.execution.ExecutionException;
import com.intellij.execution.configurations.GeneralCommandLine;
import com.intellij.execution.process.CapturingProcessHandler;
import com.intellij.execution.process.ProcessOutput;
import com.intellij.openapi.diagnostic.Logger;
import com.intellij.openapi.util.text.StringUtil;
import com.r4intellij.RHelpersLocator;
import com.r4intellij.RPsiUtils;
import com.r4intellij.interpreter.RInterpreterService;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.ArrayList;
import java.util.Collections;

public class RHelperUtil {

    public static final Logger LOG = Logger.getInstance(LocalRUtil.class.getName());


    public static String getHelperOutput(String helper) {
        final String path = RInterpreterService.getInstance().getInterpreterPath();
        if (StringUtil.isEmptyOrSpaces(path)) {
            LOG.info("Path to interpreter didn't set");
            return null;
        }
        final String helperPath = RHelpersLocator.getHelperPath(helper);
        try {
            final Process process = new GeneralCommandLine(path, "--slave", "-f", helperPath).createProcess();
            final CapturingProcessHandler processHandler = new CapturingProcessHandler(process);
            final ProcessOutput output = processHandler.runProcess(5 * RPsiUtils.MINUTE);
            if (output.getExitCode() != 0) {
                LOG.error("Failed to run script. Exit code: " + output.getExitCode());
                LOG.error(output.getStderrLines());
            }
            return output.getStdout();
        } catch (ExecutionException e) {
            LOG.error(e.getMessage());
        }
        return null;
    }


    @Nullable
    public static RRunResult runHelperWithArgs(@NotNull final String helper, @NotNull final String... args) {
        final String interpreterPath = RInterpreterService.getInstance().getInterpreterPath();
        if (StringUtil.isEmptyOrSpaces(interpreterPath)) {
            LOG.info("Path to interpreter didn't set");
            return null;
        }
        final ArrayList<String> command = Lists.newArrayList(interpreterPath, " --slave", "-f ", RHelpersLocator.getHelperPath(helper),
                " --args");
        Collections.addAll(command, args);
        try {
            final Process process = new GeneralCommandLine(command).createProcess();
            final CapturingProcessHandler processHandler = new CapturingProcessHandler(process, null, StringUtil.join(command, " "));
            final ProcessOutput output = processHandler.runProcess(5 * RPsiUtils.MINUTE);
            if (output.getExitCode() != 0) {
                LOG.error("Failed to run script. Exit code: " + output.getExitCode());
                LOG.error(output.getStderrLines());
            }
            return new RRunResult(StringUtil.join(command, " "), output);
        } catch (ExecutionException e) {
            LOG.error(e.getMessage());
        }
        return null;
    }


    public static class RRunResult {
        private final String myCommand;
        private final String myStdOut;
        private final String myStdErr;
        private int myExitCode;


        public RRunResult(@NotNull String command, @NotNull ProcessOutput output) {
            this.myCommand = command;
            this.myExitCode = output.getExitCode();
            this.myStdOut = output.getStdout();
            this.myStdErr = output.getStderr();
        }


        @NotNull
        public String getCommand() {
            return myCommand;
        }


        @NotNull
        public String getStdOut() {
            return myStdOut;
        }


        @NotNull
        public String getStdErr() {
            return myStdErr;
        }


        public int getExitCode() {
            return myExitCode;
        }
    }
}
