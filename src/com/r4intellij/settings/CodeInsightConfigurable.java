package com.r4intellij.settings;

import com.intellij.openapi.options.BaseConfigurable;
import com.intellij.openapi.options.Configurable;
import com.intellij.openapi.options.ConfigurationException;
import com.intellij.openapi.options.SearchableConfigurable;
import org.jetbrains.annotations.Nls;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import javax.swing.*;

/**
 * @author Holger Brandl
 */
public class CodeInsightConfigurable extends BaseConfigurable implements SearchableConfigurable, Configurable.NoScroll {
    private JPanel settingsPanel;


    @NotNull
    @Override
    public String getId() {
        return getClass().getName();
    }


    @Nls
    @Override
    public String getDisplayName() {
        return "Code Insight";
    }


    @Nullable
    @Override
    public String getHelpTopic() {
        return null;
    }


    @Nullable
    @Override
    public JComponent createComponent() {
        return settingsPanel;
    }


    @Override
    public void apply() throws ConfigurationException {
        RCodeInsightSettings instance = RCodeInsightSettings.getInstance();
        // todo implement me
    }


    @Override
    public void reset() {
        RCodeInsightSettings instance = RCodeInsightSettings.getInstance();

        // todo implement me
    }
}
