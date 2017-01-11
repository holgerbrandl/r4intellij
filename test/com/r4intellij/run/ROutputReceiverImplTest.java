package com.r4intellij.run;

import com.intellij.execution.process.ProcessHandler;
import com.intellij.execution.process.ProcessOutputTypes;
import com.r4intellij.debugger.data.RLanguageConstants;
import org.junit.Test;
import org.mockito.InOrder;

import static org.mockito.Mockito.*;

public class ROutputReceiverImplTest {

  @Test
  public void outputWithoutLineBreak() {
    final ProcessHandler processHandler = mock(ProcessHandler.class);
    final ROutputReceiverImpl outputReceiver = new ROutputReceiverImpl(processHandler);
    final String output = "output";

    outputReceiver.receiveOutput(output);

    final InOrder inOrder = inOrder(processHandler);
    inOrder.verify(processHandler).notifyTextAvailable(output, ProcessOutputTypes.STDOUT);
    inOrder.verify(processHandler).notifyTextAvailable(RLanguageConstants.LINE_SEPARATOR, ProcessOutputTypes.STDOUT);

    verifyNoMoreInteractions(processHandler);
  }

  @Test
  public void outputWithLineBreak() {
    final ProcessHandler processHandler = mock(ProcessHandler.class);
    final ROutputReceiverImpl outputReceiver = new ROutputReceiverImpl(processHandler);
    final String output = "output" + RLanguageConstants.LINE_SEPARATOR;

    outputReceiver.receiveOutput(output);

    verify(processHandler).notifyTextAvailable(output, ProcessOutputTypes.STDOUT);
    verifyNoMoreInteractions(processHandler);
  }

  @Test
  public void errorWithoutLineBreak() {
    final ProcessHandler processHandler = mock(ProcessHandler.class);
    final ROutputReceiverImpl outputReceiver = new ROutputReceiverImpl(processHandler);
    final String error = "error";

    outputReceiver.receiveError(error);

    final InOrder inOrder = inOrder(processHandler);
    inOrder.verify(processHandler).notifyTextAvailable(error, ProcessOutputTypes.STDERR);
    inOrder.verify(processHandler).notifyTextAvailable(RLanguageConstants.LINE_SEPARATOR, ProcessOutputTypes.STDERR);

    verifyNoMoreInteractions(processHandler);
  }

  @Test
  public void errorWithLineBreak() {
    final ProcessHandler processHandler = mock(ProcessHandler.class);
    final ROutputReceiverImpl outputReceiver = new ROutputReceiverImpl(processHandler);
    final String error = "error" + RLanguageConstants.LINE_SEPARATOR;

    outputReceiver.receiveError(error);

    verify(processHandler).notifyTextAvailable(error, ProcessOutputTypes.STDERR);
    verifyNoMoreInteractions(processHandler);
  }
}