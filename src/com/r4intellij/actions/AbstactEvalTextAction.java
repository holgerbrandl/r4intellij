package com.r4intellij.actions;

import com.intellij.openapi.actionSystem.AnAction;
import com.intellij.openapi.actionSystem.AnActionEvent;
import com.intellij.openapi.actionSystem.DataKeys;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.util.text.StringUtil;


/**
 * DOCUMENT ME!
 *
 * @author Holger Brandl
 */
public abstract class AbstactEvalTextAction extends AnAction {

    public void actionPerformed(AnActionEvent e) {
        Editor ed = e.getData(DataKeys.EDITOR);
        if (ed == null) {
            return;
        }


        String selectedText = ed.getSelectionModel().getSelectedText();
        if (selectedText == null || selectedText.isEmpty()) {
            ed.getSelectionModel().selectWordAtCaret(true);
            selectedText = ed.getSelectionModel().getSelectedText();
        }

        if (!StringUtil.isEmptyOrSpaces(selectedText)) {
            RunSelectedTextOrLineAction.push2R(getEvalCmd(selectedText));
        }

    }

    protected abstract String getEvalCmd(String selectedText);
}
