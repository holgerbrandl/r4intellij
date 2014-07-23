/*
 * Copyright 2011 Holger Brandl
 *
 * This code is licensed under BSD. For details see
 * http://www.opensource.org/licenses/bsd-license.php
 */

package com.r4intellij.editor;

import com.intellij.codeInsight.template.impl.DefaultLiveTemplatesProvider;
import org.jetbrains.annotations.NonNls;


/**
 * Add some live templates for R
 *
 * @author Holger Brandl
 */
public class LiveTemplateProvider implements DefaultLiveTemplatesProvider {

    private static final
    @NonNls
    String[] DEFAULT_TEMPLATES = new String[]{
            "/liveTemplates/rtemplates",
            "/liveTemplates/surround"
    };


    public String[] getDefaultLiveTemplateFiles() {
        return DEFAULT_TEMPLATES;
    }


    @Override
    public String[] getHiddenLiveTemplateFiles() {
        return null;
    }
}
