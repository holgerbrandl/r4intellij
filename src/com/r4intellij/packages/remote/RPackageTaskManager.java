package com.r4intellij.packages.remote;

import com.intellij.execution.ExecutionException;
import com.intellij.notification.Notification;
import com.intellij.notification.NotificationListener;
import com.intellij.notification.NotificationType;
import com.intellij.notification.NotificationsManager;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.progress.ProgressIndicator;
import com.intellij.openapi.progress.ProgressManager;
import com.intellij.openapi.progress.Task;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.Ref;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.util.Function;
import com.intellij.webcore.packaging.InstalledPackage;
import com.intellij.webcore.packaging.PackageManagementService;
import com.intellij.webcore.packaging.PackagesNotificationPanel;
import com.intellij.webcore.packaging.RepoPackage;
import org.jetbrains.annotations.NotNull;

import javax.swing.event.HyperlinkEvent;
import java.util.ArrayList;
import java.util.List;

/**
 * @author avesloguzova
 */
public class RPackageTaskManager {

    private final Project myProject;
    private final TaskListener myListener;


    public RPackageTaskManager(@NotNull final Project project, @NotNull final TaskListener listener) {
        myProject = project;
        myListener = listener;
    }


    public void install(@NotNull final RepoPackage pkg) {
        ProgressManager.getInstance().run(new InstallTask(myProject, myListener, pkg));
    }


    public void update(@NotNull final RepoPackage pkg) {
        ProgressManager.getInstance().run(new UpdateTask(myProject, myListener, pkg));
    }


    public void uninstall(@NotNull final List<InstalledPackage> installedPackages) {
        ProgressManager.getInstance().run(new UninstallTask(myProject, myListener, installedPackages));
    }


    public interface TaskListener {
        void started();


        void finished(@NotNull final List<ExecutionException> exceptions);
    }


    public abstract static class PackagingTask extends Task.Backgroundable {

        private static final String PACKAGING_GROUP_ID = "Packaging";
        @NotNull
        private TaskListener myListener;


        PackagingTask(@NotNull final Project project, @NotNull final String title, @NotNull final TaskListener listener) {
            super(project, title);
            myListener = listener;
        }


        @Override
        public void run(@NotNull final ProgressIndicator indicator) {
            taskStarted(indicator);
            taskFinished(runTask(indicator));
        }


        protected void taskStarted(@NotNull final ProgressIndicator indicator) {
            final Notification[] notifications =
                    NotificationsManager.getNotificationsManager().getNotificationsOfType(Notification.class, getProject());
            for (Notification notification : notifications) {
                notification.expire();
            }
            indicator.setText(getTitle() + "...");
            ApplicationManager.getApplication().invokeLater(new Runnable() {
                @Override
                public void run() {
                    myListener.started();
                }
            });
        }


        protected void taskFinished(@NotNull final List<ExecutionException> exceptions) {
            final Ref<Notification> notificationRef = new Ref<Notification>(null);
            if (exceptions.isEmpty()) {
                notificationRef.set(new Notification(PACKAGING_GROUP_ID, getSuccessTitle(), getSuccessDescription(),
                        NotificationType.INFORMATION, null));
            } else {
                final PackageManagementService.ErrorDescription description = RPackageManagementService.toErrorDescription(exceptions);
                if (description != null) {
                    final String firstLine = getTitle() + ": error occurred.";
                    final NotificationListener listener = new NotificationListener() {
                        @Override
                        public void hyperlinkUpdate(@NotNull Notification notification,
                                                    @NotNull HyperlinkEvent event) {
                            final String title = StringUtil.capitalizeWords(getFailureTitle(), true);
                            PackagesNotificationPanel.showError(title, description);
                        }
                    };
                    notificationRef.set(new Notification(PACKAGING_GROUP_ID, getFailureTitle(), firstLine + " <a href=\"xxx\">Details...</a>",
                            NotificationType.ERROR, listener));
                }
            }
            ApplicationManager.getApplication().invokeLater(new Runnable() {
                @Override
                public void run() {
                    myListener.finished(exceptions);
                    final Notification notification = notificationRef.get();
                    if (notification != null) {
                        notification.notify(myProject);
                    }
                }
            });
        }


        @NotNull
        protected abstract List<ExecutionException> runTask(@NotNull ProgressIndicator indicator);


        @NotNull
        protected abstract String getSuccessTitle();


        @NotNull
        protected abstract String getSuccessDescription();


        @NotNull
        protected abstract String getFailureTitle();
    }


    public static class InstallTask extends PackagingTask {
        final RepoPackage myPackage;


        InstallTask(@NotNull final Project project,
                    @NotNull final TaskListener listener,
                    @NotNull final RepoPackage repoPackage) {
            super(project, "Install package", listener);
            myPackage = repoPackage;
        }


        @NotNull
        @Override
        protected List<ExecutionException> runTask(@NotNull ProgressIndicator indicator) {
            final List<ExecutionException> exceptions = new ArrayList<ExecutionException>();
            try {
                RepoUtils.installPackage(myPackage);
            } catch (ExecutionException e) {
                exceptions.add(e);
            }
            return exceptions;
        }


        @NotNull
        @Override
        protected String getSuccessTitle() {
            return "Package installed successfully";
        }


        @NotNull
        @Override
        protected String getSuccessDescription() {
            return "Installed package " + myPackage.getName();
        }


        @NotNull
        @Override
        protected String getFailureTitle() {
            return "Install package failed";
        }
    }


    public static class UpdateTask extends PackagingTask {
        final RepoPackage myPackage;


        UpdateTask(@NotNull final Project project,
                   @NotNull final TaskListener listener,
                   @NotNull final RepoPackage repoPackage) {
            super(project, "Update package", listener);
            myPackage = repoPackage;
        }


        @NotNull
        @Override
        protected List<ExecutionException> runTask(@NotNull ProgressIndicator indicator) {
            final List<ExecutionException> exceptions = new ArrayList<ExecutionException>();
            try {
                RepoUtils.updatePackage(myPackage, RepoUtils.R_UPDATE_PACKAGE);
            } catch (ExecutionException e) {
                exceptions.add(e);
            }
            return exceptions;
        }


        @NotNull
        @Override
        protected String getSuccessTitle() {
            return "Package updated successfully";
        }


        @NotNull
        @Override
        protected String getSuccessDescription() {
            return "Updated package " + myPackage.getName();
        }


        @NotNull
        @Override
        protected String getFailureTitle() {
            return "Update package failed";
        }
    }


    public static class UninstallTask extends PackagingTask {
        private List<InstalledPackage> myPackages;


        UninstallTask(@NotNull final Project project,
                      @NotNull final TaskListener listener,
                      @NotNull final List<InstalledPackage> packages) {
            super(project, "Uninstall packages", listener);
            myPackages = packages;
        }


        @NotNull
        @Override
        protected List<ExecutionException> runTask(@NotNull ProgressIndicator indicator) {
            final List<ExecutionException> exceptions = new ArrayList<ExecutionException>();
            try {
                for (InstalledPackage pckg : myPackages) RepoUtils.uninstallPackage(pckg);
            } catch (ExecutionException e) {
                exceptions.add(e);
            }
            return exceptions;
        }


        @NotNull
        @Override
        protected String getSuccessTitle() {
            return "Packages uninstalled successfully";
        }


        @NotNull
        @Override
        protected String getSuccessDescription() {
            final String packagesString = StringUtil.join(myPackages, new Function<InstalledPackage, String>() {
                @Override
                public String fun(InstalledPackage pkg) {
                    return "'" + pkg.getName() + "'";
                }
            }, ", ");
            return "Uninstalled packages: " + packagesString;
        }


        @NotNull
        @Override
        protected String getFailureTitle() {
            return "Uninstall packages failed";
        }
    }
}
