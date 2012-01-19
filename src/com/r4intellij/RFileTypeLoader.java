/*
 * Copyright 2011 Holger Brandl
 *
 * This code is licensed under BSD. For details see
 * http://www.opensource.org/licenses/bsd-license.php
 */

package com.r4intellij;

import com.intellij.openapi.fileTypes.FileTypeConsumer;
import com.intellij.openapi.fileTypes.FileTypeFactory;
import com.r4intellij.lang.RFileType;
import org.jetbrains.annotations.NotNull;


public class RFileTypeLoader extends FileTypeFactory {

    public static final RFileType R_FILE_TYPE = new RFileType();

    public void createFileTypes(@NotNull FileTypeConsumer consumer) {
        consumer.consume(R_FILE_TYPE, RFileType.DEFAULT_EXTENSION);
    }
}
