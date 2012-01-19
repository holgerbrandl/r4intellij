/*
 * Copyright 2011 Holger Brandl
 *
 * This code is licensed under BSD. For details see
 * http://www.opensource.org/licenses/bsd-license.php
 */

package com.r4intellij.lang;

import com.intellij.lang.Language;
import com.intellij.openapi.fileTypes.LanguageFileType;
import com.intellij.openapi.util.IconLoader;
import com.r4intellij.RLanguage;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;


/**
 * The file type implementation for R files.
 *
 * @author Holger Brandl
 */
public class RFileType extends LanguageFileType {

    public static final RFileType R_FILE_TYPE = new RFileType();
    public static final Language R_LANGUAGE = R_FILE_TYPE.getLanguage();

    /**
     * The default file extension of R scripts.
     */
    public static final String DEFAULT_EXTENSION = "R";

    public RFileType() {
        super(RLanguage.INSTANCE);
    }

    @NotNull
    public String getName() {
        return "R";
    }

    @NotNull
    public String getDescription() {
        return "R scripts";
    }

    @NotNull
    public String getDefaultExtension() {
        return DEFAULT_EXTENSION;
    }

    public Icon getIcon() {
        return IconLoader.findIcon("/icons/r16_icon.png");
    }
}
