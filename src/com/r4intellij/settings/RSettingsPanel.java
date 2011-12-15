/*
 * Copyright 2011 Holger Brandl
 *
 * This code is licensed under BSD. For details see
 * http://www.opensource.org/licenses/bsd-license.php
 */

package com.r4intellij.settings;

import org.jetbrains.annotations.NotNull;

import javax.swing.*;


public class RSettingsPanel {

    private JComponent rootPanel;
    private JTextPane addComplTerms;


    public JComponent getPanel() {
        return rootPanel;
    }

    public void load(@NotNull RSettings settings) {
        addComplTerms.setText(settings.addCompletionTerms);
    }

    public boolean isModified(@NotNull RSettings settings) {
        return !settings.addCompletionTerms.equals(settings.addCompletionTerms);
//                || !settings.arcInitializationFile.equals(arcInitializationFileField.getText());
    }

    public void save(@NotNull RSettings settings) {
        settings.addCompletionTerms = addComplTerms.getText();
    }
}
