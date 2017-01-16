package com.r4intellij.run.debug;

import com.intellij.execution.ExecutionException;
import com.intellij.execution.ExecutionResult;
import com.intellij.execution.configurations.RunProfile;
import com.intellij.execution.configurations.RunProfileState;
import com.intellij.execution.executors.DefaultDebugExecutor;
import com.intellij.execution.runners.ExecutionEnvironment;
import com.intellij.execution.runners.GenericProgramRunner;
import com.intellij.execution.ui.ExecutionConsole;
import com.intellij.execution.ui.RunContentDescriptor;
import com.intellij.openapi.fileEditor.FileDocumentManager;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.vfs.LocalFileSystem;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.util.ConcurrencyUtil;
import com.intellij.xdebugger.XDebugProcess;
import com.intellij.xdebugger.XDebugProcessStarter;
import com.intellij.xdebugger.XDebugSession;
import com.intellij.xdebugger.XDebuggerManager;
import com.r4intellij.debugger.RDebugger;
import com.r4intellij.debugger.ROutputReceiver;
import com.r4intellij.debugger.evaluator.RDebuggerEvaluatorFactoryImpl;
import com.r4intellij.debugger.evaluator.RExpressionHandlerImpl;
import com.r4intellij.debugger.frame.RValueModifierFactoryImpl;
import com.r4intellij.debugger.frame.RValueModifierHandlerImpl;
import com.r4intellij.debugger.frame.RVarsLoaderFactoryImpl;
import com.r4intellij.debugger.function.RFunctionDebuggerFactoryImpl;
import com.r4intellij.run.RCommandLineState;
import com.r4intellij.run.ROutputReceiverImpl;
import com.r4intellij.run.RXProcessHandler;
import com.r4intellij.run.configuration.RRunConfiguration;
import com.r4intellij.run.debug.resolve.RResolvingSession;
import com.r4intellij.run.debug.resolve.RResolvingSessionImpl;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

public class RDebugRunner extends GenericProgramRunner {

    @NotNull
    private static final String FILE_IS_NOT_FOUND = "File is not found [path: %s]";

    @NotNull
    private static final String RUNNER_ID = "RDebugRunner";

    @NotNull
    private static final String EXECUTOR_NAME = "RDebugBackground";


    @NotNull
    @Override
    public String getRunnerId() {
        return RUNNER_ID;
    }


    @Override
    public boolean canRun(@NotNull final String executorId, @NotNull final RunProfile profile) {
        return executorId.equals(DefaultDebugExecutor.EXECUTOR_ID) && profile instanceof RRunConfiguration;
    }


    @Nullable
    @Override
    protected RunContentDescriptor doExecute(@NotNull final RunProfileState state, @NotNull final ExecutionEnvironment environment)
            throws ExecutionException {
        FileDocumentManager.getInstance().saveAllDocuments();

        final Project project = environment.getProject();
        final ExecutionResult executionResult = getExecutionResult(state, environment);

        final RXProcessHandler processHandler = (RXProcessHandler) executionResult.getProcessHandler();
        final ROutputReceiver outputReceiver = new ROutputReceiverImpl(processHandler);

        final RRunConfiguration runConfiguration = (RRunConfiguration) environment.getRunProfile();
        final String scriptPath = runConfiguration.getScriptPath();

        final XDebugSession session = XDebuggerManager.getInstance(project).startSession(
                environment,
                createDebugProcessStarter(
                        processHandler,
                        executionResult.getExecutionConsole(),
                        createDebugger(processHandler, outputReceiver, scriptPath),
                        outputReceiver,
                        createResolvingSession(
                                project,
                                getVirtualFile(scriptPath)
                        )
                )
        );

        return session.getRunContentDescriptor();
    }


    @NotNull
    private ExecutionResult getExecutionResult(@NotNull final RunProfileState state, @NotNull final ExecutionEnvironment environment)
            throws ExecutionException {
        final RCommandLineState commandLineState = (RCommandLineState) state;

        return commandLineState.execute(environment.getExecutor(), this);
    }


    @NotNull
    private XDebugProcessStarter createDebugProcessStarter(@NotNull final RXProcessHandler processHandler,
                                                           @NotNull final ExecutionConsole executionConsole,
                                                           @NotNull final RDebugger debugger,
                                                           @NotNull final ROutputReceiver outputReceiver,
                                                           @NotNull final RResolvingSession resolvingSession) {
        return new XDebugProcessStarter() {
            @NotNull
            @Override
            public XDebugProcess start(@NotNull final XDebugSession session) throws ExecutionException {
                return new RDebugProcess(
                        session,
                        processHandler,
                        executionConsole,
                        debugger,
                        outputReceiver,
                        resolvingSession,
                        ConcurrencyUtil.newSingleThreadExecutor(EXECUTOR_NAME)
                );
            }
        };
    }


    @NotNull
    private RDebugger createDebugger(@NotNull final RXProcessHandler processHandler,
                                     @NotNull final ROutputReceiver outputReceiver,
                                     @NotNull final String scriptPath) throws ExecutionException {
        try {
            return new RDebugger(
                    processHandler,
                    new RFunctionDebuggerFactoryImpl(),
                    new RVarsLoaderFactoryImpl(processHandler, outputReceiver),
                    new RDebuggerEvaluatorFactoryImpl(),
                    new BufferedReader(new FileReader(scriptPath)),
                    outputReceiver,
                    new RExpressionHandlerImpl(),
                    new RValueModifierFactoryImpl(),
                    new RValueModifierHandlerImpl()
            );
        } catch (final IOException e) {
            throw new ExecutionException(e);
        }
    }


    @NotNull
    private RResolvingSession createResolvingSession(@NotNull final Project project, @NotNull final VirtualFile virtualFile)
            throws ExecutionException {
        try {
            return new RResolvingSessionImpl(project, virtualFile);
        } catch (final IOException e) {
            throw new ExecutionException(e);
        }
    }


    @NotNull
    private VirtualFile getVirtualFile(@NotNull final String scriptPath) throws ExecutionException {
        final VirtualFile result = LocalFileSystem.getInstance().findFileByPath(scriptPath);

        if (result == null) {
            throw new ExecutionException(
                    String.format(FILE_IS_NOT_FOUND, scriptPath)
            );
        }

        return result;
    }
}
