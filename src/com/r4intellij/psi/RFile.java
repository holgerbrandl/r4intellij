/*
 * Copyright 2011 Holger Brandl
 *
 * This code is licensed under BSD. For details see
 * http://www.opensource.org/licenses/bsd-license.php
 */

package com.r4intellij.psi;

import com.intellij.extapi.psi.PsiFileBase;
import com.intellij.openapi.fileTypes.FileType;
import com.intellij.psi.FileViewProvider;
import com.r4intellij.RFileTypeLoader;
import com.r4intellij.file.RFileType;
import org.jetbrains.annotations.NotNull;


/**
 * PSI element for an Arc file
 *
 * @author Holger Brandl
 */
public class RFile extends PsiFileBase {

    public RFile(FileViewProvider viewProvider) {
        super(viewProvider, RFileType.R_LANGUAGE);
    }

    @NotNull
    public FileType getFileType() {
        return RFileTypeLoader.R_FILE_TYPE;
    }
}
