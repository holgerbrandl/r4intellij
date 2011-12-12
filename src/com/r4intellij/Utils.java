/*
 * Copyright 2011 Holger Brandl
 *
 * This code is licensed under BSD. For details see
 * http://www.opensource.org/licenses/bsd-license.php
 */

package com.r4intellij;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;


/**
 * DOCUMENT ME!
 *
 * @author Holger Brandl
 */
public class Utils {

    /**
     * @param filePath the name of the file to open. Not sure if it can accept URLs or just filenames. Path handling could be better, and buffer sizes are hardcoded
     */
    public static String readFileAsString(String filePath) {
        try {
            StringBuilder fileData = new StringBuilder(1000);
            BufferedReader reader = new BufferedReader(
                    new FileReader(filePath));
            char[] buf = new char[1024];
            int numRead = 0;
            while ((numRead = reader.read(buf)) != -1) {
                fileData.append(buf, 0, numRead);
            }
            reader.close();
            return fileData.toString();
        } catch (IOException e) {
            throw new RuntimeException("could not read file " + filePath);
        }
    }


    public static boolean isWindowsPlatform() {
        String os = System.getProperty("os.name");
        return os != null && os.startsWith("Windows");
    }


    public static boolean isMacOSX() {
        String osName = System.getProperty("os.name");
        return osName.startsWith("Mac OS X");
    }


    public static boolean isLinux() {
        return !isWindowsPlatform() & !isMacOSX();
    }

}
