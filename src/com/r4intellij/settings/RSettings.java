/*
 * Copyright 2011 Holger Brandl
 *
 * This snippet is licensed under BSD. For details see
 * http://www.opensource.org/licenses/bsd-license.php
 */

package com.r4intellij.settings;

import com.intellij.openapi.components.PersistentStateComponent;
import com.intellij.openapi.components.ServiceManager;
import com.intellij.openapi.components.State;
import com.intellij.openapi.components.Storage;
import com.intellij.openapi.util.RoamingTypeDisabled;
import com.intellij.util.xmlb.annotations.AbstractCollection;
import com.intellij.util.xmlb.annotations.Tag;

import java.util.ArrayList;
import java.util.List;


/**
 * Settings of our R plugins
 */
@State(
        name = "RSettings",
        storages = {
                @Storage(
                        id = "R",
                        file = "$APP_CONFIG$/R-settings.xml"
                )}
)
public class RSettings implements PersistentStateComponent<RSettings>, RoamingTypeDisabled {

    public String addCompletionTerms = "subset;summary;library;install.packages;head;tail";

    // custom eval actions
    public List<EvalActionPref> evalActionPrefs = new ArrayList<EvalActionPref>();

    {
        evalActionPrefs.add(new EvalActionPref("head+nrow", "head($snippet$); nrow($snippet$);", "meta alt H"));
        evalActionPrefs.add(new EvalActionPref("structure", "str($snippet$);", "meta alt S"));
        evalActionPrefs.add(new EvalActionPref("head+tail", "head($snippet$); tail($snippet$);", "meta alt T"));
        evalActionPrefs.add(new EvalActionPref("summarize", "summarize($snippet$);", "meta alt T"));
    }

    public RSettings getState() {
        return this;
    }

    public void loadState(RSettings that) {
        this.addCompletionTerms = that.addCompletionTerms;
        this.evalActionPrefs = that.getEvalActionPrefs();
    }

    public static RSettings getInstance() {
        return ServiceManager.getService(RSettings.class);
    }

    @Tag("evalActionPrefs")
    @AbstractCollection(surroundWithTag = false)
    public List<EvalActionPref> getEvalActionPrefs() {
        return evalActionPrefs;
    }

    public void setEvalActionPrefs(List<EvalActionPref> evalActionPrefs) {
        this.evalActionPrefs = evalActionPrefs;
    }
}
