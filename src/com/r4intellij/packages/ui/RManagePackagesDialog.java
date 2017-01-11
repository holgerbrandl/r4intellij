package com.r4intellij.packages.ui;

import com.intellij.openapi.project.Project;
import com.intellij.webcore.packaging.ManagePackagesDialog;
import com.intellij.webcore.packaging.PackageManagementService;
import com.r4intellij.packages.RPackageManagementService;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;


public class RManagePackagesDialog extends ManagePackagesDialog {
  public RManagePackagesDialog(@NotNull final Project project, @NotNull final PackageManagementService packageManagementService,
                               @NotNull PackageManagementService.Listener packageListener) {
    super(project, packageManagementService, packageListener);
    final JComponent panel = createCenterPanel();
    replaceListeners(panel != null ? panel.getComponents() : new Component[0], project, packageManagementService);
  }

  private void replaceListeners(Component[] components, final Project project, final PackageManagementService packageManagementService) {
    for (Component component : components) {
      if (component instanceof JButton) {
        JButton button = (JButton)component;
        if (button.getText().contains("Manage Repositories") && button.isVisible()) {
          for (ActionListener listener : button.getActionListeners()) {
            button.removeActionListener(listener);
          }
          button.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
              RManageRepoDialog dialog = new RManageRepoDialog(project, (RPackageManagementService)packageManagementService);
              dialog.show();
            }
          });
        }
      }
      else {
        if (component instanceof JPanel) {
          replaceListeners(((JPanel)component).getComponents(), project, packageManagementService);
        }
      }
    }
  }
}
