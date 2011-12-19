/*
 * Copyright 2011 Holger Brandl
 *
 * This code is licensed under BSD. For details see
 * http://www.opensource.org/licenses/bsd-license.php
 */

package com.r4intellij.actions;

/**
 * A shortcut to figure out the structure of a document in R
 *
 * @author Holger Brandl
 */
public class ShowStructureEvalAction extends AbstactEvalTextAction {


    @Override
    protected String getEvalCmd(String selectedText) {
        return "str(" + selectedText + ");";
    }
}
