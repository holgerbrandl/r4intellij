/*
 * Copyright 2011 Holger Brandl
 *
 * This code is licensed under BSD. For details see
 * http://www.opensource.org/licenses/bsd-license.php
 */

package com.r4intellij.actions;

import com.intellij.openapi.actionSystem.CustomShortcutSet;


/**
 * A shortcut to figure out the structure of a document in R
 *
 * @author Holger Brandl
 */
public class ShowStructureEvalAction extends AbstactEvalTextAction {


    public ShowStructureEvalAction(String name, String description, CustomShortcutSet shortcuts) {
        super(name, description, shortcuts);
    }

    @Override
    protected String getEvalCmd(String selectedText) {
        return "str(" + selectedText + ");";
    }
}
