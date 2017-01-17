package com.r4intellij.packages;

import com.google.common.collect.Lists;
import com.intellij.execution.ExecutionException;
import com.intellij.execution.configurations.GeneralCommandLine;
import com.intellij.execution.process.CapturingProcessHandler;
import com.intellij.execution.process.ProcessOutput;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.webcore.packaging.InstalledPackage;
import com.intellij.webcore.packaging.RepoPackage;
import com.r4intellij.RPsiUtils;
import com.r4intellij.interpreter.RInterpreterService;
import org.jetbrains.annotations.NotNull;

import java.util.ArrayList;
import java.util.List;

class PackageInstallUtils {

    private static final String R_INSTALL_PACKAGE = "r-packages/r-packages-install.r";
    private static final String R_UPDATE_PACKAGE = "r-packages/r-packages-update.r";


    static void installPackage(@NotNull RepoPackage repoPackage)
            throws ExecutionException {
        List<String> args = RepoUtils.getHelperRepositoryArguments();
        args.add(0, repoPackage.getName());
        final RHelperUtil.RRunResult result = RHelperUtil.runHelperWithArgs(R_INSTALL_PACKAGE, args.toArray(new String[args.size()]));
        if (result == null) {
            throw new ExecutionException("Please, specify path to the R executable.");
        }
        final String stderr = result.getStdErr();
        if (!stderr.contains(String.format("DONE (%s)", repoPackage.getName()))) {
            throw new RExecutionException("Some error during the installation", result.getCommand(), result.getStdOut(), result.getStdErr(),
                    result.getExitCode());
        }
    }


    static void updatePackage(@NotNull RepoPackage repoPackage)
            throws ExecutionException {
        List<String> args = RepoUtils.getHelperRepositoryArguments();
        args.add(0, repoPackage.getName());
        final RHelperUtil.RRunResult result = RHelperUtil.runHelperWithArgs(R_UPDATE_PACKAGE, args.toArray(new String[args.size()]));
        if (result == null) {
            throw new ExecutionException("Please, specify path to the R executable.");
        }
        final String stderr = result.getStdErr();
        if (!stderr.contains(String.format("DONE (%s)", repoPackage.getName()))) {
            throw new RExecutionException("Some error during the installation", result.getCommand(), result.getStdOut(), result.getStdErr(),
                    result.getExitCode());
        }
    }


    static void uninstallPackage(List<InstalledPackage> repoPackage) throws ExecutionException {
        final String path = RInterpreterService.getInstance().getInterpreterPath();
        if (StringUtil.isEmptyOrSpaces(path)) {
            throw new ExecutionException("Please, specify path to the R executable.");
        }
        final ArrayList<String> arguments = Lists.newArrayList(path, "CMD", "REMOVE");
        for (InstalledPackage aRepoPackage : repoPackage) {
            arguments.add(aRepoPackage.getName());
        }
//        final Process process = new GeneralCommandLine(arguments).createProcess();

        final CapturingProcessHandler processHandler = new CapturingProcessHandler(new GeneralCommandLine(arguments));
        final ProcessOutput output = processHandler.runProcess(5 * RPsiUtils.MINUTE);
        if (output.getExitCode() != 0) {
            throw new RExecutionException("Can't remove package", StringUtil.join(arguments, " "), output.getStdout(),
                    output.getStderr(), output.getExitCode());
        }
    }

}
