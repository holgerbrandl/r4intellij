package com.r4intellij.run;

import com.intellij.execution.ExecutionException;
import com.intellij.execution.configurations.GeneralCommandLine;
import com.intellij.execution.process.ColoredProcessHandler;
import com.intellij.execution.process.ProcessOutputTypes;
import com.intellij.execution.process.RunnerWinProcess;
import com.intellij.execution.process.UnixProcessManager;
import com.intellij.openapi.diagnostic.Logger;
import com.intellij.openapi.util.Key;
import com.intellij.openapi.util.SystemInfo;
import com.intellij.util.io.BaseDataReader;
import com.intellij.util.io.BaseOutputReader;
import com.r4intellij.debugger.exception.RDebuggerException;
import com.r4intellij.debugger.executor.RExecutionResult;
import com.r4intellij.debugger.executor.RExecutionResultCalculator;
import com.r4intellij.debugger.executor.RExecutor;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.jvnet.winp.WinProcess;

import java.io.IOException;
import java.io.OutputStreamWriter;
import java.io.Reader;
import java.util.LinkedList;
import java.util.concurrent.Future;

import static com.r4intellij.debugger.data.RLanguageConstants.LINE_SEPARATOR;

public class RXProcessHandler extends ColoredProcessHandler implements RExecutor {

  @NotNull
  private static final Logger LOGGER = Logger.getInstance(RXProcessHandler.class);

  @NotNull
  private static final Key SERVICE_KEY = ProcessOutputTypes.STDERR;

  @NotNull
  private final RExecutionResultCalculator myResultCalculator;

  private final boolean myPrintIO;

  @NotNull
  private final StringBuilder myOutputBuffer;

  @NotNull
  private final StringBuilder myErrorBuffer;

  @NotNull
  private final OutputStreamWriter myWriter;

  @NotNull
  private final LinkedList<Listener> myListeners;

  @Nullable
  private Reader myOutputReader;

  @Nullable
  private Reader myErrorReader;

  private int myExecuteCounter;

  public RXProcessHandler(@NotNull final GeneralCommandLine commandLine,
                          @NotNull final RExecutionResultCalculator resultCalculator,
                          final boolean printIO)
    throws ExecutionException {
    super(commandLine);

    myResultCalculator = resultCalculator;
    myPrintIO = printIO;

    myOutputBuffer = new StringBuilder();
    myErrorBuffer = new StringBuilder();

    myWriter = new OutputStreamWriter(getProcess().getOutputStream());

    myListeners = new LinkedList<Listener>();

    myOutputReader = null;
    myErrorReader = null;
    myExecuteCounter = 0;
  }

  @NotNull
  @Override
  public RExecutionResult execute(@NotNull final String command) throws RDebuggerException {
    try {
      myWriter.write(command);
      myWriter.write(LINE_SEPARATOR);
      myWriter.flush();

      synchronized (myOutputBuffer) {
        waitForOutput();

        synchronized (myErrorBuffer) {
          waitForError();

          final RExecutionResult result = myResultCalculator.calculate(myOutputBuffer, myErrorBuffer.toString());

          myExecuteCounter++;

          printIO(command, result);

          myOutputBuffer.setLength(0);
          myErrorBuffer.setLength(0);

          return result;
        }
      }
    }
    catch (final IOException e) {
      throw new RDebuggerException(e);
    }
    catch (final InterruptedException e) {
      throw new RDebuggerException(e);
    }
  }

  @Override
  public void startNotify() {
    super.startNotify();

    for (final Listener listener : myListeners) {
      listener.onInitialized();
    }
  }

  public void addListener(@NotNull final Listener listener) {
    myListeners.add(listener);
  }

  public void removeListener(@NotNull final Listener listener) {
    myListeners.remove(listener);
  }

  @NotNull
  @Override
  protected BaseDataReader createOutputDataReader(@NotNull final BaseDataReader.SleepingPolicy sleepingPolicy) {
    myOutputReader = super.createProcessOutReader();

    return new RXBaseOutputReader(myOutputReader, sleepingPolicy, myOutputBuffer, "output stream of " + myCommandLine);
  }

  @NotNull
  @Override
  protected BaseDataReader createErrorDataReader(@NotNull final BaseDataReader.SleepingPolicy sleepingPolicy) {
    myErrorReader = super.createProcessErrReader();

    return new RXBaseOutputReader(myErrorReader, sleepingPolicy, myErrorBuffer, "error stream of " + myCommandLine);
  }

  @Override
  protected void doDestroyProcess() {
    // reworked version of com.intellij.execution.process.impl.OSProcessManagerImpl#killProcessTree

    if (SystemInfo.isUnix) {
      UnixProcessManager.sendSignalToProcessTree(getProcess(), UnixProcessManager.SIGTERM);
    }
    else if (SystemInfo.isWindows) {
      convertToWinProcess(getProcess()).killRecursively(); // TODO [xdbg][check]
    }
    else {
      LOGGER.warn("Unexpected OS. Process will be destroyed using Java API");

      getProcess().destroy();
    }
  }

  @Override
  protected void onOSProcessTerminated(final int exitCode) {
    final String errorBuffer = waitAndCopyErrorBuffer();

    for (final Listener listener : myListeners) {
      listener.onTerminated(errorBuffer);
    }

    super.onOSProcessTerminated(exitCode);
  }

  private void waitForOutput() throws IOException, InterruptedException {
    assert myOutputReader != null;

    synchronized (myOutputBuffer) {
      while (myOutputReader.ready() || !myResultCalculator.isComplete(myOutputBuffer)) {
        myOutputBuffer.wait();
      }
    }
  }

  private void waitForError() throws IOException, InterruptedException {
    assert myErrorReader != null;

    synchronized (myErrorBuffer) {
      while (myErrorReader.ready()) {
        myErrorBuffer.wait();
      }
    }
  }

  private void printIO(@NotNull final String command, @NotNull final RExecutionResult result) {
    if (myPrintIO) {
      printIO("COMMAND", command);

      printIO("TYPE", result.getType().toString());
      printIO("OUTPUT", result.getOutput());
      printIO("RESULT", result.getResultRange().substring(result.getOutput()));

      printIO("ERROR", result.getError());
    }
  }

  @NotNull
  private String waitAndCopyErrorBuffer() {
    try {
      waitForError();
    }
    catch (IOException ignored) {
    }
    catch (InterruptedException ignored) {
    }

    synchronized (myErrorBuffer) {
      return myErrorBuffer.toString();
    }
  }

  @NotNull
  private WinProcess convertToWinProcess(@NotNull final Process process) {
    // copied from com.intellij.execution.process.impl.OSProcessManagerImpl#createWinProcess

    if (process instanceof RunnerWinProcess) {
      return new WinProcess(((RunnerWinProcess)process).getOriginalProcess());
    }
    else {
      return new WinProcess(process);
    }
  }

  private void printIO(@NotNull final String title, @NotNull final String message) {
    notifyTextAvailable(title, SERVICE_KEY);
    notifyTextAvailable(" #", SERVICE_KEY);
    notifyTextAvailable(Integer.toString(myExecuteCounter), SERVICE_KEY);
    notifyTextAvailable(":\n", SERVICE_KEY);
    notifyTextAvailable(message, SERVICE_KEY);
    notifyTextAvailable("\n\n", SERVICE_KEY);
  }

  public interface Listener {

    void onInitialized();

    void onTerminated(@NotNull final String errorBuffer);
  }

  private class RXBaseOutputReader extends BaseOutputReader {

    @NotNull
    private final StringBuilder myBuffer;

    public RXBaseOutputReader(@NotNull final Reader reader,
                              @NotNull final SleepingPolicy sleepingPolicy,
                              @NotNull final StringBuilder buffer,
                              @NotNull final String presentableName) {
      super(reader, sleepingPolicy);

      myBuffer = buffer;

      start(presentableName);
    }

    @Override
    protected void onTextAvailable(@NotNull final String text) {
      synchronized (myBuffer) {
        myBuffer.append(text);
        myBuffer.notify();
      }
    }

    @NotNull
    @Override
    protected Future<?> executeOnPooledThread(@NotNull final Runnable runnable) {
      return RXProcessHandler.this.executeOnPooledThread(runnable);
    }
  }
}
