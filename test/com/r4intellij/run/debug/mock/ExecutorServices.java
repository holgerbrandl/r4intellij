package com.r4intellij.run.debug.mock;

import com.intellij.util.ConcurrencyUtil;
import org.jetbrains.annotations.NotNull;

import java.util.Collection;
import java.util.List;
import java.util.concurrent.*;

public final class ExecutorServices {

  @NotNull
  public static final ExecutorService SINGLE_EXECUTOR = ConcurrencyUtil.newSingleThreadExecutor("RDebuggerTestBackground");

  @NotNull
  public static final ExecutorService ILLEGAL_EXECUTOR = new ExecutorService() {

    @Override
    public void shutdown() {
      throw new IllegalStateException("Shutdown shouldn't be called");
    }

    @NotNull
    @Override
    public List<Runnable> shutdownNow() {
      throw new IllegalStateException("ShutdownNow shouldn't be called");
    }

    @Override
    public boolean isShutdown() {
      throw new IllegalStateException("IsShutdown shouldn't be called");
    }

    @Override
    public boolean isTerminated() {
      throw new IllegalStateException("IsTerminated shouldn't be called");
    }

    @Override
    public boolean awaitTermination(final long timeout, @NotNull final TimeUnit unit) throws InterruptedException {
      throw new IllegalStateException("AwaitTermination shouldn't be called");
    }

    @NotNull
    @Override
    public <T> Future<T> submit(@NotNull final Callable<T> task) {
      throw new IllegalStateException("Submit shouldn't be called");
    }

    @NotNull
    @Override
    public <T> Future<T> submit(@NotNull final Runnable task, final T result) {
      throw new IllegalStateException("Submit shouldn't be called");
    }

    @NotNull
    @Override
    public Future<?> submit(@NotNull final Runnable task) {
      throw new IllegalStateException("Submit shouldn't be called");
    }

    @NotNull
    @Override
    public <T> List<Future<T>> invokeAll(@NotNull final Collection<? extends Callable<T>> tasks) throws InterruptedException {
      throw new IllegalStateException("InvokeAll shouldn't be called");
    }

    @NotNull
    @Override
    public <T> List<Future<T>> invokeAll(@NotNull final Collection<? extends Callable<T>> tasks,
                                         final long timeout,
                                         @NotNull final TimeUnit unit)
      throws InterruptedException {
      throw new IllegalStateException("InvokeAll shouldn't be called");
    }

    @NotNull
    @Override
    public <T> T invokeAny(@NotNull final Collection<? extends Callable<T>> tasks) throws InterruptedException, ExecutionException {
      throw new IllegalStateException("InvokeAny shouldn't be called");
    }

    @Override
    public <T> T invokeAny(@NotNull final Collection<? extends Callable<T>> tasks, final long timeout, @NotNull final TimeUnit unit)
      throws InterruptedException, ExecutionException, TimeoutException {
      throw new IllegalStateException("InvokeAny shouldn't be called");
    }

    @Override
    public void execute(@NotNull final Runnable command) {
      throw new IllegalStateException("Execute shouldn't be called");
    }
  };
}
