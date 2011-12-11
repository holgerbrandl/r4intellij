/*
 * Copyright 2010 Holger Brandl
 * File: BashFileType.java, Class: BashFileType
 * Last modified: 2010-06-30
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.r4intellij.file;

import com.intellij.lang.Language;
import com.intellij.openapi.fileTypes.LanguageFileType;
import com.intellij.openapi.util.IconLoader;
import com.r4intellij.RLanguage;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;


/**
 * The file type implementation for Bash files.
 * <p/>
 * Date: 22.03.2009
 * Time: 11:08:04
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
