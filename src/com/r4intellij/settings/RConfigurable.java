/*
 * Copyright 2011 Holger Brandl
 *
 * This code is licensed under BSD. For details see
 * http://www.opensource.org/licenses/bsd-license.php
 */

package com.r4intellij.settings;

import com.intellij.openapi.options.Configurable;
import com.intellij.openapi.options.ConfigurationException;
import com.intellij.openapi.util.IconLoader;
import com.r4intellij.misc.Strings;
import org.jetbrains.annotations.Nullable;

import javax.swing.*;


public class RConfigurable implements Configurable {

    private RSettingsPanel panel;


    public String getDisplayName() {
        return Strings.message("plugin.name");
    }


    @Nullable
    public Icon getIcon() {
        return IconLoader.findIcon("/icons/r_logo_16.png");
//        return ArcIcons.ARC_CONFIG_ICON;
    }


    @Nullable
    public String getHelpTopic() {
        return null;
    }


    public JComponent createComponent() {
        panel = new RSettingsPanel();
        panel.load(RSettings.getInstance());
        return panel.getPanel();
    }


    public boolean isModified() {
        return panel.isModified(RSettings.getInstance());
    }


    public void apply() throws ConfigurationException {
        panel.save(RSettings.getInstance());
    }


    public void reset() {
        panel.load(RSettings.getInstance());
    }


    public void disposeUIResources() {
        // Anything???
    }
}
