package com.r4intellij.run.debug;

import com.intellij.execution.process.ProcessHandler;
import com.intellij.execution.ui.ExecutionConsole;
import com.intellij.openapi.diagnostic.Logger;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.ui.Messages;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.xdebugger.XDebugProcess;
import com.intellij.xdebugger.XDebugSession;
import com.intellij.xdebugger.XExpression;
import com.intellij.xdebugger.XSourcePosition;
import com.intellij.xdebugger.breakpoints.XBreakpointHandler;
import com.intellij.xdebugger.breakpoints.XBreakpointProperties;
import com.intellij.xdebugger.breakpoints.XLineBreakpoint;
import com.intellij.xdebugger.evaluation.XDebuggerEditorsProvider;
import com.intellij.xdebugger.evaluation.XDebuggerEvaluator;
import com.intellij.xdebugger.frame.XExecutionStack;
import com.intellij.xdebugger.frame.XStackFrame;
import com.intellij.xdebugger.frame.XSuspendContext;
import com.intellij.xdebugger.frame.XValue;
import com.r4intellij.debugger.RDebugger;
import com.r4intellij.debugger.ROutputReceiver;
import com.r4intellij.debugger.data.RInterpreterConstants;
import com.r4intellij.debugger.exception.RDebuggerException;
import com.r4intellij.debugger.exception.RRuntimeException;
import com.r4intellij.debugger.frame.RStackFrame;
import com.r4intellij.run.RProcessUtils;
import com.r4intellij.run.RXProcessHandler;
import com.r4intellij.run.debug.resolve.RResolvingSession;
import com.r4intellij.run.debug.stack.RXStack;
import com.r4intellij.run.debug.stack.RXValue;
import com.r4intellij.run.graphics.RGraphicsUtils;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.*;
import java.util.concurrent.ExecutorService;

// TODO [xdbg][test]
class RDebugProcess extends XDebugProcess implements RXProcessHandler.Listener {

  @NotNull
  private static final Logger LOGGER = Logger.getInstance(RDebugProcess.class);

  @NotNull
  private final RXProcessHandler myProcessHandler;

  @NotNull
  private final ExecutionConsole myExecutionConsole;

  @NotNull
  private final RDebugger myDebugger;

  @NotNull
  private final ROutputReceiver myOutputReceiver;

  @NotNull
  private final RXStack myStack;

  @NotNull
  private final ExecutorService myExecutor;

  @NotNull
  private final Map<XSourcePositionWrapper, XLineBreakpoint<XBreakpointProperties>> myBreakpoints;

  @NotNull
  private final Set<XSourcePositionWrapper> myTempBreakpoints;

  @NotNull
  private final REditorsProvider myEditorsProvider;

  @NotNull
  private final XBreakpointHandler[] myBreakpointHandlers;

  public RDebugProcess(@NotNull final XDebugSession session,
                       @NotNull final RXProcessHandler processHandler,
                       @NotNull final ExecutionConsole executionConsole,
                       @NotNull final RDebugger debugger,
                       @NotNull final ROutputReceiver outputReceiver,
                       @NotNull final RResolvingSession resolvingSession,
                       @NotNull final ExecutorService executor) {
    super(session);

    myProcessHandler = processHandler;
    myExecutionConsole = executionConsole;

    myDebugger = debugger;
    myOutputReceiver = outputReceiver;
    myStack = new RXStack(myDebugger.getStack(), resolvingSession, executor);
    myExecutor = executor;

    myBreakpoints = new HashMap<XSourcePositionWrapper, XLineBreakpoint<XBreakpointProperties>>();
    myTempBreakpoints = new HashSet<XSourcePositionWrapper>();

    myEditorsProvider = new REditorsProvider();
    myBreakpointHandlers = new XBreakpointHandler[]{new RXLineBreakpointHandler()};

    myProcessHandler.addListener(this);
  }

  @NotNull
  @Override
  public ExecutionConsole createConsole() {
    return myExecutionConsole;
  }

  @NotNull
  @Override
  public XDebuggerEditorsProvider getEditorsProvider() {
    return myEditorsProvider;
  }

  @NotNull
  @Override
  public XBreakpointHandler<?>[] getBreakpointHandlers() {
    return myBreakpointHandlers;
  }

  @Override
  public void sessionInitialized() {
    RGraphicsUtils.getGraphicsState(getSession().getProject()).reset();
  }

  @Nullable
  @Override
  protected ProcessHandler doGetProcessHandler() {
    return myProcessHandler;
  }

  @Override
  public void startStepOver() {
    myExecutor.execute(
      new Runnable() {
        @Override
        public void run() {
          try {
            final List<RStackFrame> stack = myDebugger.getStack();
            final int targetDepth = stack.size();

            do {
              if (!advance()) return;

              myStack.update();
            }
            while (!isBreakpoint() && stack.size() > targetDepth);

            showDebugInformation();
          }
          catch (final RDebuggerException e) {
            handleException(e);
          }
        }
      }
    );
  }

  @Override
  public void startStepInto() {
    myExecutor.execute(
      new Runnable() {
        @Override
        public void run() {
          try {
            if (!advance()) return;

            myStack.update();

            showDebugInformation();
          }
          catch (final RDebuggerException e) {
            handleException(e);
          }
        }
      }
    );
  }

  @Override
  public void startStepOut() {
    myExecutor.execute(
      new Runnable() {
        @Override
        public void run() {
          try {
            final List<RStackFrame> stack = myDebugger.getStack();
            final int targetDepth = stack.size() - 1;

            do {
              if (!advance()) return;

              myStack.update();
            }
            while (!isBreakpoint() && stack.size() > targetDepth);

            showDebugInformation();
          }
          catch (final RDebuggerException e) {
            handleException(e);
          }
        }
      }
    );
  }

  @Override
  public void resume() {
    myExecutor.execute(
      new Runnable() {
        @Override
        public void run() {
          try {
            do {
              if (!advance()) return;

              myStack.update();
            }
            while (!isBreakpoint());

            showDebugInformation();
          }
          catch (final RDebuggerException e) {
            handleException(e);
          }
        }
      }
    );
  }

  @Override
  public void runToPosition(@NotNull final XSourcePosition position) {
    final Project project = getSession().getProject();
    final VirtualFile file = position.getFile();
    final int line = position.getLine();

    if (!RLineBreakpointUtils.canPutAt(project, file, line)) {
      Messages.showErrorDialog(
        project,
        "There is no executable code at " + file.getName() + ":" + (line + 1),
        "RUN TO CURSOR"
      );

      getSession().positionReached(myStack.getSuspendContext());

      return;
    }

    myTempBreakpoints.add(new XSourcePositionWrapper(position));

    resume();
  }

  @Override
  public void stop() {
    myExecutor.shutdownNow();
  }

  @Override
  public void onInitialized() {
    myExecutor.execute(
      new Runnable() {
        @Override
        public void run() {
          try {
            for (final String command : RInterpreterConstants.INIT_DEBUG_COMMANDS) {
              myProcessHandler.execute(command);
            }

            RProcessUtils.executeInitGraphicsCommands(getSession().getProject(), myProcessHandler);
          }
          catch (final RDebuggerException e) {
            handleException(e);
          }
        }
      }
    );

    resume();
  }

  @Override
  public void onTerminated(@NotNull final String errorBuffer) {
    if (!errorBuffer.isEmpty()) {
      myOutputReceiver.receiveError(errorBuffer);
    }
  }

  private boolean advance() throws RDebuggerException {
    final boolean executed = myDebugger.advance();

    if (!executed) {
      getSession().stop();
    }

    return executed;
  }

  private boolean isBreakpoint() {
    final XSourcePositionWrapper wrapper = new XSourcePositionWrapper(getCurrentPosition());

    return myBreakpoints.containsKey(wrapper) || myTempBreakpoints.contains(wrapper);
  }

  private void showDebugInformation() {
    final XSourcePositionWrapper wrapper = new XSourcePositionWrapper(getCurrentPosition());
    final XLineBreakpoint<XBreakpointProperties> breakpoint = myBreakpoints.get(wrapper);

    final XDebugSession session = getSession();
    final XSuspendContext suspendContext = myStack.getSuspendContext();

    if (breakpoint != null) {
      final XDebuggerEvaluator evaluator = getActiveEvaluator();
      final XExpression conditionExpression = breakpoint.getConditionExpression();

      if (evaluator != null && conditionExpression != null) {
        evaluator.evaluate(
          conditionExpression,
          new EvaluationCallback(breakpoint, suspendContext),
          null
        );
      }
      else if (!session.breakpointReached(breakpoint, null, suspendContext)) { // 2nd arg is printed to console when breakpoint is reached
        resume();
      }
    }
    else {
      session.positionReached(suspendContext);

      myTempBreakpoints.remove(wrapper);
    }

    RGraphicsUtils.getGraphicsState(session.getProject()).refresh(true);
  }

  private void handleException(@NotNull final RDebuggerException e) {
    if (e instanceof RRuntimeException) {
      if (e.getMessage().isEmpty()) { // sometimes error message couldn't be loaded in time
        myOutputReceiver.receiveError("Debug has been interrupted because of runtime error");
      }

      getSession().stop();

      return;
    }

    if (myExecutor.isShutdown() && e.getCause() instanceof InterruptedException) {
      return;
    }

    LOGGER.error(e);
  }

  @NotNull
  private XSourcePosition getCurrentPosition() {
    final XExecutionStack stack = myStack.getSuspendContext().getActiveExecutionStack();
    assert stack != null;

    final XStackFrame frame = stack.getTopFrame();
    assert frame != null;

    return frame.getSourcePosition();  // TODO [xdbg][null]
  }

  @Nullable
  private XDebuggerEvaluator getActiveEvaluator() {
    final XExecutionStack activeExecutionStack = myStack.getSuspendContext().getActiveExecutionStack();
    assert activeExecutionStack != null; // see RXSuspendContext#getActiveExecutionStack()

    final XStackFrame topFrame = activeExecutionStack.getTopFrame();

    return topFrame == null ? null : topFrame.getEvaluator();
  }

  private static class XSourcePositionWrapper {

    @NotNull
    private final XSourcePosition myPosition;

    private XSourcePositionWrapper(@NotNull final XSourcePosition position) {
      myPosition = position;
    }

    @Override
    public boolean equals(@Nullable final Object o) {
      if (o == this) return true;
      if (o == null || getClass() != o.getClass()) return false;

      final XSourcePositionWrapper wrapper = (XSourcePositionWrapper)o;

      return myPosition.getLine() == wrapper.myPosition.getLine() &&
             myPosition.getFile().getPath().equals(wrapper.myPosition.getFile().getPath());
    }

    @Override
    public int hashCode() {
      return 31 * myPosition.getLine() + myPosition.getFile().getPath().hashCode();
    }
  }

  private class RXLineBreakpointHandler extends XBreakpointHandler<XLineBreakpoint<XBreakpointProperties>> {

    public RXLineBreakpointHandler() {
      super(RLineBreakpointType.class);
    }

    @Override
    public void registerBreakpoint(@NotNull final XLineBreakpoint<XBreakpointProperties> breakpoint) {
      assert breakpoint.getSourcePosition() != null;

      myBreakpoints.put(
        new XSourcePositionWrapper(breakpoint.getSourcePosition()),
        breakpoint
      );
    }

    @Override
    public void unregisterBreakpoint(@NotNull final XLineBreakpoint<XBreakpointProperties> breakpoint, final boolean temporary) {
      assert breakpoint.getSourcePosition() != null;

      myBreakpoints.remove(
        new XSourcePositionWrapper(breakpoint.getSourcePosition())
      );
    }
  }

  private class EvaluationCallback implements XDebuggerEvaluator.XEvaluationCallback {

    private final int myPrefixLength = "[1] ".length();

    @NotNull
    private final XLineBreakpoint<XBreakpointProperties> myBreakpoint;

    @NotNull
    private final XSuspendContext mySuspendContext;

    public EvaluationCallback(@NotNull final XLineBreakpoint<XBreakpointProperties> breakpoint,
                              @NotNull final XSuspendContext suspendContext) {
      myBreakpoint = breakpoint;
      mySuspendContext = suspendContext;
    }

    @Override
    public void evaluated(@NotNull final XValue result) {
      if (result instanceof RXValue) {
        final String value = ((RXValue)result).getValue();
        final boolean evaluatedToTrue = value.length() > myPrefixLength && Boolean.parseBoolean(value.substring(myPrefixLength));

        if (evaluatedToTrue &&
            getSession()
              .breakpointReached(myBreakpoint, null, mySuspendContext)) { // 2 arg is printed to console when breakpoint is reached
          return;
        }
      }

      resume();
    }

    @Override
    public void errorOccurred(@NotNull final String errorMessage) {
      LOGGER.info(errorMessage);
    }
  }
}
