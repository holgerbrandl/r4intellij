package com.r4intellij.packages;

import com.google.common.collect.Lists;
import com.intellij.execution.ExecutionException;
import com.intellij.execution.configurations.GeneralCommandLine;
import com.intellij.execution.process.CapturingProcessHandler;
import com.intellij.execution.process.ProcessOutput;
import com.intellij.openapi.diagnostic.Logger;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.util.PathUtil;
import com.r4intellij.RPsiUtils;
import com.r4intellij.settings.RSettings;
import org.jetbrains.annotations.NonNls;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.io.File;
import java.util.ArrayList;
import java.util.Collections;

public class RHelperUtil {

    public static final PluginResourceFile INSTALL_TIDYVERSE = new PluginResourceFile("install_tidyverse.r");

    public static final Logger LOG = Logger.getInstance(RHelperUtil.class.getName());


    @Nullable
    public static ProcessOutput getProcessOutput(@NotNull final String scriptText) {
        String interpreter = RSettings.getInstance().getInterpreterPath();

        final String path = interpreter;
        if (path == null) {
            return null;
        }


        String[] getPckgsCmd = new String[]{interpreter, "--vanilla", "--quiet", "--slave", "-e", scriptText};

        try {
            final CapturingProcessHandler processHandler = new CapturingProcessHandler(new GeneralCommandLine(getPckgsCmd));
            return processHandler.runProcess(5 * RPsiUtils.MINUTE);
        } catch (Throwable e) {
            LOG.info("Failed to run R executable: \n" +
                    "Interpreter path " + path + "0\n" +
                    "Exception occurred: " + e.getMessage());
        }

        return null;
    }


    @Deprecated
    public static String runCommandWithCat(String cmd) {
        return runCommand("cat(" + cmd + ", sep='\\\\n')").trim();
    }


    public static String runCommand(String cmd) {
        ProcessOutput processOutput = getProcessOutput(cmd);
        if (processOutput != null && processOutput.getExitCode() == 0)
            return processOutput.getStdout();

        else return "";
    }


    @Nullable
    public static String getHelperOutput(PluginResourceFile helper, String... args) {
        RRunResult rRunResult = runHelperWithArgs(helper, args);
        return rRunResult != null ? rRunResult.getStdOut() : null;
    }


    @Nullable
    public static RRunResult runHelperWithArgs(@NotNull final PluginResourceFile helper, @NotNull final String... args) {
        final String interpreterPath = RSettings.getInstance().getInterpreterPath();

        if (StringUtil.isEmptyOrSpaces(interpreterPath)) {
            LOG.info("Path to interpreter didn't set");
            return null;
        }

        final ArrayList<String> command = Lists.newArrayList(
                interpreterPath,
                "--slave",
                "-f", helper.getFile().getAbsolutePath(),
                "--args");

        Collections.addAll(command, args);


        try {
            GeneralCommandLine gcl = new GeneralCommandLine(command);
//            LOG.info("running helper with: " + gcl.getCommandLineString());
            final Process process = gcl.createProcess();
            final CapturingProcessHandler processHandler = new CapturingProcessHandler(process, null, StringUtil.join(command, " "));

            final ProcessOutput output = processHandler.runProcess(5 * RPsiUtils.MINUTE);

            if (output.getExitCode() != 0) {
                LOG.warn("Failed to run script. Exit code: " + output.getExitCode());
                LOG.warn(output.getStderr());
            }

            return new RRunResult(StringUtil.join(command, " "), output);
        } catch (ExecutionException e) {
            LOG.error(e.getMessage());
        }

        return null;
    }


    private static File getHelpersRoot() {
        @NonNls String jarPath = PathUtil.getJarPathForClass(RHelperUtil.class);
        if (jarPath.endsWith(".jar")) {
            final File jarFile = new File(jarPath);

            LOG.assertTrue(jarFile.exists(), "jar file cannot be null");
            return jarFile.getParentFile().getParentFile();
        }

        return new File(jarPath);
    }


    public static File getResourceFile(String resourceName) {
        return new File(getHelpersRoot(), resourceName);
    }


    public static class PluginResourceFile {
        private final String relativePath;


        public PluginResourceFile(String fileName) {
            this.relativePath = fileName;
        }


        public File getFile() {
            return getResourceFile(relativePath);
        }
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
