package com.r4intellij.packages.remote.ui;

import com.intellij.openapi.project.Project;
import com.intellij.openapi.wm.ToolWindow;
import com.intellij.openapi.wm.ToolWindowFactory;
import com.intellij.ui.content.Content;
import com.intellij.ui.content.ContentFactory;
import com.intellij.webcore.packaging.PackagesNotificationPanel;
import com.r4intellij.packages.remote.RPackageManagementService;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;

/**
 * @author avesloguzova
 */
public class RPackagesToolWindowFactory implements ToolWindowFactory {

    public RPackagesToolWindowFactory() {
    }


    @Override
    public void createToolWindowContent(@NotNull final Project project, @NotNull final ToolWindow toolWindow) {
        final PackagesNotificationPanel notificationPanel = new PackagesNotificationPanel();
        final RInstalledPackagesPanel packagesPanel = new RInstalledPackagesPanel(project, notificationPanel);
        packagesPanel.setBorder(BorderFactory.createEmptyBorder(4, 0, 0, 0));
        packagesPanel.updatePackages(new RPackageManagementService(project));
        final ContentFactory contentFactory = ContentFactory.SERVICE.getInstance();
        final Content content = contentFactory.createContent(packagesPanel, "", false);
        toolWindow.getContentManager().addContent(content);
    }
}
