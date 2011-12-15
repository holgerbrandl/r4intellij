/*
 * Copyright 2011 Holger Brandl
 *
 * This code is licensed under BSD. For details see
 * http://www.opensource.org/licenses/bsd-license.php
 */

package com.r4intellij.settings;

import com.intellij.openapi.components.PersistentStateComponent;
import com.intellij.openapi.components.ServiceManager;
import com.intellij.openapi.components.State;
import com.intellij.openapi.components.Storage;
import com.intellij.openapi.util.RoamingTypeDisabled;


/**
 * Settings of our R plugins
 */
@State(
        name = "RSettings",
        storages = {
                @Storage(
                        id = "arc",
                        file = "$APP_CONFIG$/R-settings.xml"
                )}
)
public class RSettings implements PersistentStateComponent<RSettings>, RoamingTypeDisabled {

    public String addCompletionTerms = "subset;summary;library;install.packages;head;tail";

    public RSettings getState() {
        return this;
    }

    public void loadState(RSettings that) {
        this.addCompletionTerms = that.addCompletionTerms;
    }

    public static RSettings getInstance() {
        return ServiceManager.getService(RSettings.class);
    }
}
