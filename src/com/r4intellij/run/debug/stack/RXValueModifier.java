package com.r4intellij.run.debug.stack;

import com.intellij.xdebugger.frame.XValueModifier;
import com.r4intellij.debugger.frame.RValueModifier;
import org.jetbrains.annotations.NotNull;

import java.util.concurrent.ExecutorService;

// TODO [xdbg][test]
class RXValueModifier extends XValueModifier {

  @NotNull
  private final RValueModifier myModifier;

  @NotNull
  private final String myName;

  @NotNull
  private final ExecutorService myExecutor;

  public RXValueModifier(@NotNull final RValueModifier modifier, @NotNull final String name, @NotNull final ExecutorService executor) {
    myModifier = modifier;
    myName = name;
    myExecutor = executor;
  }

  @Override
  public void setValue(@NotNull final String expression, @NotNull final XModificationCallback callback) {
    myExecutor.execute(
      new Runnable() {
        @Override
        public void run() {
          myModifier.setValue(
            myName,
            expression,
            new Listener(callback)
          );
        }
      }
    );
  }

  private static class Listener implements RValueModifier.Listener {

    @NotNull
    private final XModificationCallback myCallback;

    public Listener(@NotNull final XModificationCallback callback) {
      myCallback = callback;
    }

    @Override
    public void onSuccess() {
      myCallback.valueModified();
    }

    @Override
    public void onError(@NotNull final String error) {
      myCallback.errorOccurred(error);
    }

    @Override
    public void onError(@NotNull final Exception e) {
      onError(e.getMessage());
    }
  }
}
