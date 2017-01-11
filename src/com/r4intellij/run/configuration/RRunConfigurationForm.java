package com.r4intellij.run.configuration;

import com.intellij.execution.configuration.EnvironmentVariablesComponent;
import com.intellij.openapi.fileChooser.FileChooserDescriptorFactory;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.ui.ComponentWithBrowseButton;
import com.intellij.openapi.ui.TextComponentAccessor;
import com.intellij.openapi.ui.TextFieldWithBrowseButton;
import com.intellij.openapi.util.io.FileUtil;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.ui.RawCommandLineEditor;
import com.intellij.ui.components.JBLabel;
import com.r4intellij.RFileType;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import java.util.Map;

// TODO [run][test]
public class RRunConfigurationForm implements RRunConfigurationParams {

  private JPanel myRootPanel;

  private JBLabel myScriptPathLabel;
  private TextFieldWithBrowseButton myScriptPathField;

  private JBLabel myScriptArgsLabel;
  private RawCommandLineEditor myScriptArgsField;

  private JBLabel myWorkingDirectoryPathLabel;
  private TextFieldWithBrowseButton myWorkingDirectoryPathField;

  private EnvironmentVariablesComponent myEnvsComponent;

  public RRunConfigurationForm(@NotNull final Project project) {
    setupScriptPathField(project);
    setupScriptArgsField();
    setupWorkingDirectoryPathField(project);

    myScriptPathLabel.setAnchor(myEnvsComponent.getLabel());
    myScriptArgsLabel.setAnchor(myEnvsComponent.getLabel());
    myWorkingDirectoryPathLabel.setAnchor(myEnvsComponent.getLabel());
  }

  @NotNull
  public JComponent getPanel() {
    return myRootPanel;
  }

  @NotNull
  @Override
  public String getScriptPath() {
    return getPath(myScriptPathField);
  }

  @Override
  public void setScriptPath(@NotNull final String scriptPath) {
    setPath(myScriptPathField, scriptPath);
  }

  @NotNull
  @Override
  public String getScriptArgs() {
    return myScriptArgsField.getText().trim();
  }

  @Override
  public void setScriptArgs(@NotNull final String scriptArgs) {
    myScriptArgsField.setText(scriptArgs);
  }

  @NotNull
  @Override
  public String getWorkingDirectoryPath() {
    return getPath(myWorkingDirectoryPathField);
  }

  @Override
  public void setWorkingDirectoryPath(@NotNull final String workingDirectoryPath) {
    setPath(myWorkingDirectoryPathField, workingDirectoryPath);
  }

  @Override
  public boolean isPassParentEnvs() {
    return myEnvsComponent.isPassParentEnvs();
  }

  @Override
  public void setPassParentEnvs(final boolean passParentEnvs) {
    myEnvsComponent.setPassParentEnvs(passParentEnvs);
  }

  @NotNull
  @Override
  public Map<String, String> getEnvs() {
    return myEnvsComponent.getEnvs();
  }

  @Override
  public void setEnvs(@NotNull final Map<String, String> envs) {
    myEnvsComponent.setEnvs(envs);
  }

  private void setupScriptPathField(@NotNull final Project project) {
    final ComponentWithBrowseButton.BrowseFolderActionListener<JTextField> listener =
      new ComponentWithBrowseButton.BrowseFolderActionListener<JTextField>(
        "Select Script",
        "",
        myScriptPathField,
        project,
        FileChooserDescriptorFactory.createSingleFileDescriptor(RFileType.INSTANCE),
        TextComponentAccessor.TEXT_FIELD_WHOLE_TEXT
      ) {
        @Override
        protected void onFileChosen(@NotNull final VirtualFile chosenFile) {
          super.onFileChosen(chosenFile);

          RRunConfigurationUtils.setSuggestedWorkingDirectoryPathIfNotSpecified(RRunConfigurationForm.this);
        }
      };

    myScriptPathField.addActionListener(listener);
  }

  private void setupScriptArgsField() {
    myScriptArgsField.setDialogCaption("Script Args");
  }

  private void setupWorkingDirectoryPathField(@NotNull final Project project) {
    myWorkingDirectoryPathField.addBrowseFolderListener(
      "Select Working Directory",
      "",
      project,
      FileChooserDescriptorFactory.createSingleFolderDescriptor()
    );
  }

  @NotNull
  private String getPath(@NotNull final TextFieldWithBrowseButton field) {
    return FileUtil.toSystemIndependentName(field.getText().trim());
  }

  private void setPath(@NotNull final TextFieldWithBrowseButton field, @NotNull final String path) {
    field.setText(FileUtil.toSystemDependentName(path));
  }
}
