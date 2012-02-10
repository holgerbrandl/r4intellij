/*
 * Copyright 2011 Holger Brandl
 *
 * This code is licensed under BSD. For details see
 * http://www.opensource.org/licenses/bsd-license.php
 */

package com.r4intellij.settings;

import com.r4intellij.Utils;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import java.util.Arrays;


public class RSettingsPanel {

    private JComponent rootPanel;
    private JTextPane addComplTerms;
    private JTextField evalTitle1;
    private JTextField evalCode1;
    private JTextField evalTitle2;
    private JTextField evalTitle3;
    private JTextField evalCode2;
    private JTextField evalCode3;
    private JTextField evalTitle4;
    private JTextField evalCode4;
    private JComboBox codeEvalTarget;


    public JComponent getPanel() {
        return rootPanel;
    }

    public void load(@NotNull RSettings settings) {
        addComplTerms.setText(settings.addCompletionTerms);

        for (int i = 0; i < settings.getEvalActionPrefs().size(); i++) {
            EvalActionPref evalActionPref = settings.getEvalActionPrefs().get(i);
            switch (i) {
                case 0:
                    evalTitle1.setText(evalActionPref.getName());
                    evalCode1.setText(evalActionPref.getCode());
                    break;
                case 1:
                    evalTitle2.setText(evalActionPref.getName());
                    evalCode2.setText(evalActionPref.getCode());
                    break;
                case 2:
                    evalTitle3.setText(evalActionPref.getName());
                    evalCode3.setText(evalActionPref.getCode());
                    break;
                case 3:
                    evalTitle4.setText(evalActionPref.getName());
                    evalCode4.setText(evalActionPref.getCode());
                    break;
            }
        }

        codeEvalTarget.setSelectedItem(settings.codeSnippetEvalTarget == null ? getEvalTargetOptions()[0] : settings.codeSnippetEvalTarget);
    }

    public boolean isModified(@NotNull RSettings settings) {
        return !settings.addCompletionTerms.equals(settings.addCompletionTerms) || true;
//                || !settings.arcInitializationFile.equals(arcInitializationFileField.getText());
    }

    public void save(@NotNull RSettings settings) {
        settings.addCompletionTerms = addComplTerms.getText();

        settings.evalActionPrefs = Arrays.asList(
                new EvalActionPref(evalTitle1.getText(), evalCode1.getText(), RSettings.SNIPACTION_1_DEF_SHORTCUT),
                new EvalActionPref(evalTitle2.getText(), evalCode2.getText(), RSettings.SNIPACTION_2_DEF_SHORTCUT),
                new EvalActionPref(evalTitle3.getText(), evalCode3.getText(), RSettings.SNIPACTION_3_DEF_SHORTCUT),
                new EvalActionPref(evalTitle4.getText(), evalCode4.getText(), RSettings.SNIPACTION_4_DEF_SHORTCUT)
        );

        settings.codeSnippetEvalTarget = codeEvalTarget.getSelectedItem().toString();
    }

    private void createUIComponents() {
        String[] evalTargetOptions = getEvalTargetOptions();
        codeEvalTarget = new JComboBox(new DefaultComboBoxModel(evalTargetOptions));
    }

    private String[] getEvalTargetOptions() {
        return Utils.isMacOSX() ? new String[]{"R", "R64", "Terminal"} : Utils.isWindowsPlatform() ? new String[]{"R"} : new String[]{""};
    }
}
