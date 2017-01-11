package com.r4intellij.run;

import com.intellij.execution.configurations.GeneralCommandLine;
import com.intellij.execution.configurations.GeneralCommandLine.ParentEnvironmentType;
import com.intellij.openapi.util.io.FileUtil;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.util.execution.ParametersListUtil;
import com.r4intellij.debugger.data.RInterpreterConstants;
import com.r4intellij.run.configuration.RRunConfiguration;
import org.jetbrains.annotations.NotNull;

import java.util.ArrayList;
import java.util.List;

final class RCommandLineCalculator {

  @NotNull
  public static GeneralCommandLine calculateCommandLine(@NotNull final String interpreterPath,
                                                        @NotNull final RRunConfiguration runConfiguration) {
    return new GeneralCommandLine(calculateCommand(interpreterPath, runConfiguration))
      .withWorkDirectory(runConfiguration.getWorkingDirectoryPath())
      .withEnvironment(runConfiguration.getEnvs())
      .withParentEnvironmentType(runConfiguration.isPassParentEnvs() ? ParentEnvironmentType.CONSOLE : ParentEnvironmentType.NONE);
  }

  @NotNull
  private static List<String> calculateCommand(@NotNull final String interpreterPath,
                                               @NotNull final RRunConfiguration runConfiguration) {
    final List<String> command = new ArrayList<String>();

    command.add(FileUtil.toSystemDependentName(interpreterPath));
    command.addAll(RInterpreterConstants.DEFAULT_PARAMETERS);

    final String scriptArgs = runConfiguration.getScriptArgs();

    if (!StringUtil.isEmptyOrSpaces(scriptArgs)) {
      command.add(RInterpreterConstants.ARGS_PARAMETER);
      command.addAll(ParametersListUtil.parse(scriptArgs));
    }

    return command;
  }
}
