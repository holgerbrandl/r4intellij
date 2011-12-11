/*
 * Copyright 2011 Holger Brandl
 *
 * This code is licensed under BSD. For details see
 * http://www.opensource.org/licenses/bsd-license.php
 */

package com.r4intellij.file;

import com.intellij.openapi.fileTypes.FileTypeConsumer;
import com.intellij.openapi.fileTypes.FileTypeFactory;
import org.jetbrains.annotations.NotNull;


/**
 * The file type loader of the R plugin.
 *
 * @author Holger Brandl
 */
public class RFileTypeFactory extends FileTypeFactory {

    public void createFileTypes(@NotNull FileTypeConsumer consumer) {
        consumer.consume(RFileType.R_FILE_TYPE, RFileType.DEFAULT_EXTENSION);

        //add more file types here:
//        consumer.consume(RFileType.R_FILE_TYPE, "whatever");
    }
}
