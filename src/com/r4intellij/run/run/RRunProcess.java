package com.r4intellij.run.run;

import com.intellij.execution.ExecutionResult;
import com.intellij.execution.process.ProcessAdapter;
import com.intellij.execution.process.ProcessEvent;
import com.intellij.execution.runners.ExecutionEnvironment;
import com.intellij.execution.runners.RunContentBuilder;
import com.intellij.execution.ui.RunContentDescriptor;
import com.intellij.openapi.diagnostic.Logger;
import com.intellij.openapi.project.Project;
import com.r4intellij.debugger.RDebuggerStringUtils;
import com.r4intellij.debugger.ROutputReceiver;
import com.r4intellij.debugger.data.RCommands;
import com.r4intellij.debugger.exception.RDebuggerException;
import com.r4intellij.debugger.executor.RExecutorUtils;
import com.r4intellij.run.ROutputReceiverImpl;
import com.r4intellij.run.RProcessUtils;
import com.r4intellij.run.RXProcessHandler;
import com.r4intellij.run.configuration.RRunConfiguration;
import com.r4intellij.run.graphics.RGraphicsUtils;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.concurrent.ExecutorService;

// TODO [run][test]
class RRunProcess {

  @NotNull
  private static final Logger LOGGER = Logger.getInstance(RRunProcess.class);

  @NotNull
  private final Project myProject;

  @NotNull
  private final ExecutionEnvironment myEnvironment;

  @NotNull
  private final ExecutionResult myExecutionResult;

  @NotNull
  private final ExecutorService myExecutor;

  public RRunProcess(@NotNull final Project project,
                     @NotNull final ExecutionEnvironment environment,
                     @NotNull final ExecutionResult executionResult,
                     @NotNull final ExecutorService executor) {
    myProject = project;
    myEnvironment = environment;
    myExecutionResult = executionResult;
    myExecutor = executor;
  }

  @NotNull
  public RunContentDescriptor getRunContentDescriptor() {
    RGraphicsUtils.getGraphicsState(myProject).reset();

    final RXProcessHandler processHandler = (RXProcessHandler)myExecutionResult.getProcessHandler();
    final String scriptPath = ((RRunConfiguration)myEnvironment.getRunProfile()).getScriptPath();

    processHandler.addListener(new InitializationProcessListener(myProject, scriptPath, processHandler, myExecutor));
    processHandler.addProcessListener(new TerminationProcessListener(myProject));

    return new RunContentBuilder(myExecutionResult, myEnvironment).showRunContent(myEnvironment.getContentToReuse());
  }

  private static class InitializationProcessListener implements RXProcessHandler.Listener {

    @NotNull
    private final Project myProject;

    @NotNull
    private final String myScriptPath;

    @NotNull
    private final RXProcessHandler myProcessHandler;

    @NotNull
    private final ExecutorService myExecutor;

    public InitializationProcessListener(@NotNull final Project project,
                                         @NotNull final String scriptPath,
                                         @NotNull final RXProcessHandler processHandler,
                                         @NotNull final ExecutorService executor) {
      myProject = project;
      myScriptPath = scriptPath;
      myProcessHandler = processHandler;
      myExecutor = executor;
    }

    @Override
    public void onInitialized() {
      myExecutor.submit(
        new Runnable() {
          @Override
          public void run() {
            final ROutputReceiver outputReceiver = new ROutputReceiverImpl(myProcessHandler);

            try {
              RProcessUtils.executeInitGraphicsCommands(myProject, myProcessHandler);

              RDebuggerStringUtils.appendResult(
                RExecutorUtils.execute(
                  myProcessHandler,
                  RCommands.sourceCommand(myScriptPath),
                  outputReceiver
                ),
                outputReceiver
              );

              RExecutorUtils.execute(
                myProcessHandler,
                RCommands.QUIT_COMMAND,
                outputReceiver
              );
            }
            catch (final RDebuggerException e) {
              LOGGER.error(e);
            }
            finally {
              myExecutor.shutdown();
            }
          }
        }
      );
    }

    @Override
    public void onTerminated(@NotNull final String errorBuffer) {
      if (!errorBuffer.isEmpty()) {
        new ROutputReceiverImpl(myProcessHandler).receiveError(errorBuffer);
      }
    }
  }

  private static class TerminationProcessListener extends ProcessAdapter {

    @NotNull
    private final Project myProject;

    public TerminationProcessListener(@NotNull final Project project) {
      myProject = project;
    }

    @Override
    public void processTerminated(@Nullable final ProcessEvent event) {
      RGraphicsUtils.getGraphicsState(myProject).refresh(false);
    }
  }
}
