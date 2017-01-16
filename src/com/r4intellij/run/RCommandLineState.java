package com.r4intellij.run;

import com.intellij.execution.ExecutionException;
import com.intellij.execution.configurations.CommandLineState;
import com.intellij.execution.configurations.GeneralCommandLine;
import com.intellij.execution.executors.DefaultDebugExecutor;
import com.intellij.execution.process.ProcessHandler;
import com.intellij.execution.process.ProcessTerminatedListener;
import com.intellij.execution.runners.ExecutionEnvironment;
import com.intellij.openapi.options.ConfigurationException;
import com.intellij.openapi.util.text.StringUtil;
import com.r4intellij.debugger.executor.RExecutionResultCalculator;
import com.r4intellij.debugger.executor.RExecutionResultCalculatorImpl;
import com.r4intellij.interpreter.RInterpreterService;
import com.r4intellij.run.configuration.RRunConfiguration;
import com.r4intellij.run.configuration.RRunConfigurationUtils;
import com.r4intellij.run.run.RRunExecutionResultCalculator;
import org.jetbrains.annotations.NotNull;

import static java.lang.Boolean.parseBoolean;

public class RCommandLineState extends CommandLineState {

    @NotNull
    private static final String IO_ENV_KEY = "ther.debugger.io";

    @NotNull
    private final RRunConfiguration myRunConfiguration;


    public RCommandLineState(@NotNull final ExecutionEnvironment environment, @NotNull final RRunConfiguration runConfiguration) {
        super(environment);

        myRunConfiguration = runConfiguration;
    }


    @NotNull
    @Override
    protected ProcessHandler startProcess() throws ExecutionException {
        checkRunConfiguration();

        final String interpreterPath = RInterpreterService.getInstance().getInterpreterPath();

        if (StringUtil.isEmptyOrSpaces(interpreterPath)) {
            throw new ExecutionException("R interpreter is not specified");
        }

        final ProcessHandler processHandler = startProcess(
                myRunConfiguration,
                RCommandLineCalculator.calculateCommandLine(
                        interpreterPath,
                        myRunConfiguration
                )
        );

        ProcessTerminatedListener.attach(processHandler, myRunConfiguration.getProject());

        return processHandler;
    }


    private void checkRunConfiguration() throws ExecutionException {
        try {
            RRunConfigurationUtils.checkConfiguration(myRunConfiguration);
        } catch (final ConfigurationException e) {
            throw new ExecutionException(e);
        }
    }


    @NotNull
    private ProcessHandler startProcess(@NotNull final RRunConfiguration runConfiguration,
                                        @NotNull final GeneralCommandLine commandLine) throws ExecutionException {
        return new RXProcessHandler(
                commandLine,
                createExecutionResultCalculator(),
                parseBoolean(runConfiguration.getEnvs().get(IO_ENV_KEY))
        );
    }


    @NotNull
    private RExecutionResultCalculator createExecutionResultCalculator() {
        if (getEnvironment().getExecutor().getId().equals(DefaultDebugExecutor.EXECUTOR_ID)) {
            return new RExecutionResultCalculatorImpl();
        } else {
            return new RRunExecutionResultCalculator();
        }
    }
}
