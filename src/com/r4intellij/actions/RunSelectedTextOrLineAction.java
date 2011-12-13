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
import com.intellij.openapi.diagnostic.Logger;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.util.text.StringUtil;
import com.r4intellij.Utils;

import java.io.IOException;


/**
 * Event handler for the "Run Selection" action within an Arc code editor - runs the currently selected text within the current REPL.
 */
public class RunSelectedTextOrLineAction extends AnAction {

    private static final Logger log = Logger.getInstance("#RunSelectedTextOrLineAction");

    public void actionPerformed(AnActionEvent e) {
        Editor ed = e.getData(DataKeys.EDITOR);
        if (ed == null) {
            return;
        }

        String text = ed.getSelectionModel().getSelectedText();
        if (StringUtil.isEmptyOrSpaces(text)) {
            ed.getSelectionModel().selectLineAtCaret();
            push2R(ed.getSelectionModel().getSelectedText().replace("\\n", ""));
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
            push2R(text);
        }

    }

    public static void push2R(String text) {
        try {
            if (Utils.isMacOSX()) {
                Runtime runtime = Runtime.getRuntime();

                String dquotesExpandedText = text.replace("\"", "\\\"");
                String evalSelection = "tell application \"R64\" to activate\n" +
                        "tell application \"R64\" to cmd \"" + dquotesExpandedText + "\"";


                String[] args = {"osascript", "-e", evalSelection};

                runtime.exec(args);
            }
        } catch (IOException e1) {
            log.error(e1);
        }
    }
}
