/*
 * Copyright 2011 Holger Brandl
 *
 * This code is licensed under BSD. For details see
 * http://www.opensource.org/licenses/bsd-license.php
 */

package com.r4intellij.actions;

import com.intellij.openapi.actionSystem.AnAction;
import com.intellij.openapi.actionSystem.AnActionEvent;
import com.intellij.openapi.actionSystem.DataKeys;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.util.text.StringUtil;
import com.r4intellij.misc.connectors.ConnectorUtils;


/**
 * Event handler for the "Run Selection" action within an Arc code editor - runs the currently selected text within the
 * current REPL.
 */
public class RunSelectedTextOrLineAction extends AnAction {

    public void actionPerformed(AnActionEvent e) {
        Editor ed = e.getData(DataKeys.EDITOR);
        if (ed == null) {
            return;
        }

        String text = ed.getSelectionModel().getSelectedText();
        if (StringUtil.isEmptyOrSpaces(text)) {
            ed.getSelectionModel().selectLineAtCaret();
            ConnectorUtils.push2R(ed.getSelectionModel().getSelectedText().replace("\\n", ""));
//
//            int caret = ed.getSelectionModel().getSelectionStart();
//            PsiFile file = e.getData(DataKeys.PSI_FILE);
//            if (file != null) {
//                for (PsiElement el : file.getChildren()) {
//                    if (el.getTextRange().contains(caret)) {
//                        if (!(el instanceof PsiWhiteSpace) && !(el instanceof PsiComment)) {
//                            push2R(el.getText());
//                        }
//                        return;
//                    }
//                }
//            }
        } else {
            ConnectorUtils.push2R(text);
        }
    }

}
