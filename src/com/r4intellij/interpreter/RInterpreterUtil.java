package com.r4intellij.interpreter;

import com.google.common.collect.Iterables;
import com.intellij.openapi.util.SystemInfo;
import com.intellij.openapi.util.io.FileUtil;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.openapi.vfs.LocalFileSystem;
import com.intellij.openapi.vfs.VirtualFile;
import org.jetbrains.annotations.NotNull;

import java.io.File;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

/**
 * Created by brandl on 1/16/17.
 */
public class RInterpreterUtil {

    private static String[] WINDOWS_PATHS = {"C:\\", "C:\\Program Files\\"};
    private static String[] UNIX_PATHS = {"/usr", "/usr/local"};
    private static String[] MAC_PATHS = {"/Library/Frameworks/R.framework/Resources", "/System/Library/Frameworks/R.framework/Resources"};


    private static List<String> suggestHomePaths() {
        final ArrayList<String> result = new ArrayList<String>();
        if (SystemInfo.isWindows) {
            addInstallationFromPaths(result, "R.exe", WINDOWS_PATHS);
            addFromPathVariable(result);
        } else if (SystemInfo.isMac) {
            addInstallationFromPaths(result, "R", MAC_PATHS);
        } else if (SystemInfo.isUnix) {
            addInstallationFromPaths(result, "R", UNIX_PATHS);
        }

        return result;
    }


    private static void addInstallationFromPaths(@NotNull final ArrayList<String> result, @NotNull final String scriptName,
                                                 @NotNull final String[] paths) {
        for (String path : paths) {
            final VirtualFile rootVDir = LocalFileSystem.getInstance().findFileByPath(path);
            if (rootVDir != null) {
                final VirtualFile rScript = rootVDir.findFileByRelativePath("bin/" + scriptName);
                if (rScript != null) {
                    result.add(FileUtil.toSystemDependentName(rScript.getPath()));
                }
            }
        }
    }


    private static void addFromPathVariable(Collection<String> result) {
        final String path = System.getenv("PATH");
        for (String pathEntry : StringUtil.split(path, ";")) {
            if (pathEntry.startsWith("\"") && pathEntry.endsWith("\"")) {
                if (pathEntry.length() < 2) continue;
                pathEntry = pathEntry.substring(1, pathEntry.length() - 1);
            }
            File f = new File(pathEntry, "R.exe");
            if (f.exists()) {
                result.add(FileUtil.toSystemDependentName(f.getPath()));
            }
        }
    }


    static String suggestHomePath() {
        return Iterables.getFirst(suggestHomePaths(), "");
    }
}
