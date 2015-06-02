/*
 * Copyright 2011 Holger Brandl
 *
 * This code is licensed under BSD. For details see
 * http://www.opensource.org/licenses/bsd-license.php
 */

package com.r4intellij.actions;

import com.intellij.openapi.actionSystem.AnAction;
import com.intellij.openapi.actionSystem.AnActionEvent;
import com.intellij.openapi.actionSystem.CustomShortcutSet;
import com.intellij.openapi.actionSystem.PlatformDataKeys;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.util.text.StringUtil;
import com.r4intellij.misc.connectors.ConnectorUtils;


/**
 * DOCUMENT ME!
 *
 * @author Holger Brandl
 */
public abstract class AbstactEvalTextAction extends AnAction {

    public AbstactEvalTextAction(String name, String description, CustomShortcutSet shortcuts) {
        super(name, description, null);
        setShortcutSet(shortcuts);
    }


    public void actionPerformed(AnActionEvent e) {
        Editor ed = e.getData(PlatformDataKeys.EDITOR);
        if (ed == null) {
            return;
        }


        String selectedText = ed.getSelectionModel().getSelectedText();
        if (selectedText == null || selectedText.isEmpty()) {
            ed.getSelectionModel().selectWordAtCaret(true);
            selectedText = ed.getSelectionModel().getSelectedText();
        }

        if (!StringUtil.isEmptyOrSpaces(selectedText)) {
            ConnectorUtils.push2R(getEvalCmd(selectedText));
        }

    }


    protected abstract String getEvalCmd(String selectedText);
}
