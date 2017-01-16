package com.r4intellij.run.configuration;

import com.intellij.openapi.options.ConfigurationException;
import com.intellij.openapi.options.SettingsEditor;
import com.intellij.openapi.project.Project;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;

class RRunConfigurationEditor extends SettingsEditor<RRunConfiguration> {

    private RRunConfigurationForm myForm;


    public RRunConfigurationEditor(@NotNull final Project project) {
        myForm = new RRunConfigurationForm(project);
    }


    @Override
    protected void resetEditorFrom(@NotNull final RRunConfiguration config) {
        RRunConfiguration.copyParams(config, myForm);
    }


    @Override
    protected void applyEditorTo(@NotNull final RRunConfiguration config) throws ConfigurationException {
        RRunConfiguration.copyParams(myForm, config);
    }


    @Override
    @NotNull
    protected JComponent createEditor() {
        return myForm.getPanel();
    }


    @Override
    protected void disposeEditor() {
        myForm = null;
    }
}
