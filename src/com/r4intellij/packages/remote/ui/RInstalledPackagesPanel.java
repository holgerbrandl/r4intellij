package com.r4intellij.packages.remote.ui;

import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.webcore.packaging.*;
import com.r4intellij.interpreter.RSkeletonGenerator;
import com.r4intellij.settings.RSettings;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

/**
 * @author avesloguzova
 */
public class RInstalledPackagesPanel extends InstalledPackagesPanel {
    public RInstalledPackagesPanel(@NotNull final Project project, @NotNull final PackagesNotificationPanel area) {
        super(project, area);
    }


    public static boolean hasInterpreterPath() {
        return StringUtil.isNotEmpty(RSettings.getInstance().getInterpreterPath());
    }


    public static boolean isPackageBase(@NotNull final InstalledPackage pkg) {
        return RSkeletonGenerator.DEFAULT_PACKAGES.contains(pkg.getName());
    }


    @Override
    protected boolean canUninstallPackage(InstalledPackage aPackage) {
        return hasInterpreterPath() && !isPackageBase(aPackage);
    }


    @Override
    @NotNull
    protected ManagePackagesDialog createManagePackagesDialog() {
        return new RManagePackagesDialog(this.myProject, this.myPackageManagementService, new PackageManagementService.Listener() {
            @Override
            public void operationStarted(String packageName) {
                myPackagesTable.setPaintBusy(true);
            }


            @Override
            public void operationFinished(String packageName, @Nullable PackageManagementService.ErrorDescription errorDescription) {
                myNotificationArea.showResult(packageName, errorDescription);
                myPackagesTable.clearSelection();
                doUpdatePackages(myPackageManagementService);
            }
        });
    }


    @Override
    protected boolean canInstallPackage(@NotNull final InstalledPackage aPackage) {
        return hasInterpreterPath();
    }


    @Override
    protected boolean canUpgradePackage(InstalledPackage aPackage) {
        return hasInterpreterPath() && !isPackageBase(aPackage);
    }
}
