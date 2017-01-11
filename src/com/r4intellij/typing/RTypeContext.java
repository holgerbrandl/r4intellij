package com.r4intellij.typing;

import com.intellij.openapi.diagnostic.Logger;
import com.intellij.openapi.project.Project;
import com.intellij.psi.util.PsiModificationTracker;
import com.r4intellij.psi.api.RPsiElement;
import com.r4intellij.typing.types.RErrorType;
import com.r4intellij.typing.types.RType;
import com.r4intellij.typing.types.RUnknownType;
import org.jetbrains.annotations.NotNull;

import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

public class RTypeContext {
  private static final Map<Project, RTypeContext> CONTEXT = new HashMap<Project, RTypeContext>();
  private static final Logger LOG = Logger.getInstance(RTypeContext.class);
  private final Map<RPsiElement, RType> cache = new HashMap<RPsiElement, RType>();
  private final Lock cacheLock = new ReentrantLock();
  private long myModificationCount = -1;

  private RTypeContext(long modificationCount) {
    myModificationCount = modificationCount;
  }

  public static Map<RPsiElement, RErrorType> getExpressionsWithError(Project project) {
    return RTypeContext.getContext(project).getExpressionsWithError();
  }

  public Map<RPsiElement, RErrorType> getExpressionsWithError() {
    Map<RPsiElement, RErrorType> errors = new HashMap<RPsiElement, RErrorType>();
    cacheLock.lock();
    for (Map.Entry<RPsiElement, RType> entry : cache.entrySet()) {
      RPsiElement element = entry.getKey();
      RType type = entry.getValue();
      if (type instanceof RErrorType) {
        errors.put(element, (RErrorType)type);
      }
    }
    cacheLock.unlock();
    return errors;
  }

  public static RType getTypeFromCache(RPsiElement element) {
    RType type = getContext(element.getProject()).getType(element);
    if (type == null) {
      LOG.error("Type for " + element.getText() + " is null. WTF");
    }
    if (type instanceof RErrorType) {
      return RUnknownType.INSTANCE;
    }
    return type;
  }

  public static void putTypeInCache(RPsiElement element, RType type) {
    getContext(element.getProject()).putType(element, type);
  }

  @NotNull
  private static RTypeContext getContext(Project project) {
    final PsiModificationTracker tracker = PsiModificationTracker.SERVICE.getInstance(project);
    RTypeContext context;
    synchronized (CONTEXT) {
      context = CONTEXT.get(project);
      final long count = tracker.getModificationCount();
      if (context == null || context.myModificationCount != count) {
        context = new RTypeContext(count);
        CONTEXT.put(project, context);
      }
    }
    return context;
  }

  private void putType(RPsiElement element, RType type) {
    cacheLock.lock();
    RType typeInCache = cache.get(element);
    if (typeInCache == null) {
      cache.put(element, type);
    }
    else if (!REvaluatingNowType.class.isInstance(typeInCache) && !typeInCache.equals(type)) {
      throw new RuntimeException("Wrong types for element " + element.toString() + " : " + typeInCache + " and " + type.toString());
    }
    cacheLock.unlock();
  }

  private RType getType(RPsiElement element) {
    REvaluatingNowType evaluatingType = new REvaluatingNowType();
    cacheLock.lock();
    RType type = cache.get(element);
    if (type != null) {
      if (type instanceof REvaluatingNowType) {
        cacheLock.unlock();
        evaluatingType = (REvaluatingNowType)type;
        try {
          //noinspection SynchronizationOnLocalVariableOrMethodParameter
          synchronized (evaluatingType) {
            if (evaluatingType.isNotRecursive()) {
              int numberOfAttempts = 0;
              while (!evaluatingType.isReady()) {
                evaluatingType.wait(5000);
                if (5 == numberOfAttempts++) { // it's sad
                  LOG.info("Possible deadlock, break waiting");
                  return RTypeProvider.buildType(element);
                }
              }
            }
          }
        }
        catch (InterruptedException e) {
          //
        }
        return evaluatingType.getResult();
      }
      cacheLock.unlock();
      return type;
    }
    cache.put(element, evaluatingType);
    cacheLock.unlock();
    type = RTypeProvider.buildType(element);
    cacheLock.lock();
    cache.put(element, type);
    cacheLock.unlock();
    evaluatingType.setResult(type);
    return type;
  }

  private static class REvaluatingNowType extends RType {
    private RType myResult;
    private volatile boolean myReady;
    private final long myThreadId;

    private REvaluatingNowType() {
      myResult = RUnknownType.INSTANCE;
      myThreadId = Thread.currentThread().getId();
    }

    @Override
    public String getCanonicalName() {
      return "evaluating now";
    }

    public RType getResult() {
      if (myResult == null) {
        LOG.error("Result type is null. WTF");
      }
      return myResult;
    }

    public synchronized void setResult(RType result) {
      myResult = result;
      myReady = true;
      notifyAll();
    }

    public boolean isReady() {
      return myReady;
    }

    public boolean isNotRecursive() {
      return Thread.currentThread().getId() != myThreadId;
    }
  }
}
