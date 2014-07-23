/*
 * Copyright 2011 Holger Brandl
 *
 * This code is licensed under BSD. For details see
 * http://www.opensource.org/licenses/bsd-license.php
 */

package com.r4intellij.actions;

import com.intellij.openapi.actionSystem.AnActionEvent;
import com.intellij.openapi.actionSystem.DefaultActionGroup;
import com.intellij.openapi.actionSystem.PlatformDataKeys;
import com.intellij.openapi.vfs.VirtualFile;


/**
 * Allows to group R code actions.
 *
 * @author Holger Brandl
 */
public class RActionGroup extends DefaultActionGroup {


    public RActionGroup() {
        super("R Code Evaluation", true);
    }


    @Override
    public void update(AnActionEvent e) {
        VirtualFile[] data = e.getData(PlatformDataKeys.VIRTUAL_FILE_ARRAY);

        if (data == null || data.length == 0) {
            e.getPresentation().setEnabled(false);
            return;
        }

        // just enable the menu if an R file is open
        e.getPresentation().setEnabled(data[0].getExtension().equals("R"));
    }
}
