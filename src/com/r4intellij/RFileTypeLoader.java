/*
 * Copyright 2011 Holger Brandl
 *
 * This code is licensed under BSD. For details see
 * http://www.opensource.org/licenses/bsd-license.php
 */

package com.r4intellij;

import com.intellij.openapi.fileTypes.FileTypeConsumer;
import com.intellij.openapi.fileTypes.FileTypeFactory;
import com.r4intellij.file.RFileType;
import org.jetbrains.annotations.NotNull;


/**
 * Created by IntelliJ IDEA.
 * User: kurtc
 * Date: Mar 2, 2009
 * Time: 9:59:50 AM
 * To change this template use File | Settings | File Templates.
 */
public class RFileTypeLoader extends FileTypeFactory {

    public static final RFileType R_FILE_TYPE = new RFileType();

    public void createFileTypes(@NotNull FileTypeConsumer consumer) {
        consumer.consume(R_FILE_TYPE, RFileType.DEFAULT_EXTENSION);
    }
}
