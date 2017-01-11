package com.r4intellij.packages;

import com.google.common.collect.Lists;
import com.intellij.execution.ExecutionException;
import com.intellij.openapi.project.Project;
import com.intellij.util.CatchingConsumer;
import com.intellij.util.containers.ContainerUtil;
import com.intellij.webcore.packaging.InstalledPackage;
import com.intellij.webcore.packaging.PackageManagementService;
import com.intellij.webcore.packaging.RepoPackage;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.Collection;
import java.util.List;

/**
 * @author avesloguzova
 */
public class RPackageManagementService extends PackageManagementService {

  @NotNull private final Project myProject;

  public RPackageManagementService(@NotNull final Project project) {
    myProject = project;
  }

  @Nullable
  public static ErrorDescription toErrorDescription(@NotNull List<ExecutionException> exceptions) {
    //noinspection LoopStatementThatDoesntLoop
    for (ExecutionException e : exceptions) {
      if (e instanceof RExecutionException) {
        RExecutionException exception = (RExecutionException)e;
        return new ErrorDescription(exception.getMessage(), exception.getCommand(), exception.getStderr(), null);
      }
      else {
        return new ErrorDescription(e.getMessage(), null, null, null);
      }
    }
    return null;
  }

  @Override
  @NotNull
  public List<String> getAllRepositories() {
    final RPackageService service = RPackageService.getInstance();
    final List<RDefaultRepository> defaultRepositories = getDefaultRepositories();
    final List<String> result = Lists.newArrayList();
    for (RDefaultRepository repository : defaultRepositories) {
      result.add(repository.getUrl());
    }
    result.addAll(service.userRepositories);
    return result;
  }

  @NotNull
  public List<RDefaultRepository> getDefaultRepositories() {
    return Lists.newArrayList(RPackagesUtil.getDefaultRepositories()); //TODO Caching of this value
  }

  public List<String> getMirrors() {
    return Lists.newArrayList(RPackagesUtil.getCRANMirrors());
  }

  public int getCRANMirror() {
    return RPackageService.getInstance().CRANMirror;
  }

  public void setCRANMirror(int index) {
    RPackageService.getInstance().CRANMirror = index;
  }

  public void setRepositories(List<RRepository> repositories) {
    final List<String> userRepositories = Lists.newArrayList();
    final List<String> defaultRepositories = Lists.newArrayList();
    for (RRepository repository : repositories) {
      if (repository instanceof RDefaultRepository) {
        defaultRepositories.add(repository.getUrl());
      }
      else {
        userRepositories.add(repository.getUrl());
      }
    }
    RPackagesUtil.setRepositories(defaultRepositories, userRepositories);
  }

  @Override
  public List<RepoPackage> getAllPackages() {
    return RPackagesUtil.getOrLoadPackages();
  }

  @Override
  public List<RepoPackage> reloadAllPackages() {
    return RPackagesUtil.loadAvailablePackages();
  }

  @Override
  public Collection<InstalledPackage> getInstalledPackages() {
    return RPackagesUtil.getInstalledPackages();
  }

  @Override
  public void installPackage(final RepoPackage repoPackage, String version, boolean forceUpgrade, String extraOptions,
                             final Listener listener, boolean installToUser) {
    final RPackageTaskManager manager = new RPackageTaskManager(myProject, new RPackageTaskManager.TaskListener() {
      @Override
      public void started() {
        listener.operationStarted(repoPackage.getName());
      }

      @Override
      public void finished(@NotNull final List<ExecutionException> exceptions) {
        listener.operationFinished(repoPackage.getName(), toErrorDescription(exceptions));
      }
    });

    if (forceUpgrade) {
      manager.update(repoPackage);
    }
    else {
      manager.install(repoPackage);
    }
  }

  @Override
  public boolean canInstallToUser() {
    return false;
  }

  @Override
  public void uninstallPackages(List<InstalledPackage> installedPackages, final Listener listener) {
    final String packageName = installedPackages.size() == 1 ? installedPackages.get(0).getName() : null;
    final RPackageTaskManager manager = new RPackageTaskManager(myProject, new RPackageTaskManager.TaskListener() {
      @Override
      public void started() {
        listener.operationStarted(packageName);
      }

      @Override
      public void finished(@NotNull final List<ExecutionException> exceptions) {
        listener.operationFinished(packageName, toErrorDescription(exceptions));
      }
    });
    manager.uninstall(installedPackages);
  }

  @Override
  public void fetchPackageVersions(String s, CatchingConsumer<List<String>, Exception> consumer) {
    consumer.consume(ContainerUtil.<String>emptyList());
  }

  @Override
  public void fetchPackageDetails(String packageName, CatchingConsumer<String, Exception> consumer) {
    RPackagesUtil.fetchPackageDetails(packageName, consumer);
  }
}
