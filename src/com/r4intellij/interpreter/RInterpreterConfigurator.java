package com.r4intellij.interpreter;

import com.intellij.openapi.module.Module;
import com.intellij.openapi.progress.PerformInBackgroundOption;
import com.intellij.openapi.progress.ProgressIndicator;
import com.intellij.openapi.progress.ProgressManager;
import com.intellij.openapi.progress.Task;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.Ref;
import com.intellij.openapi.util.SystemInfo;
import com.intellij.openapi.util.io.FileUtil;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.openapi.vfs.LocalFileSystem;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.openapi.vfs.VirtualFileManager;
import com.intellij.platform.DirectoryProjectConfigurator;
import org.jetbrains.annotations.NotNull;

import java.io.File;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

public class RInterpreterConfigurator implements DirectoryProjectConfigurator {

    private static String[] WINDOWS_PATHS = {"C:\\", "C:\\Program Files\\"};
    private static String[] UNIX_PATHS = {"/usr", "/usr/local"};
    private static String[] MAC_PATHS = {"/Library/Frameworks/R.framework/Resources", "/System/Library/Frameworks/R.framework/Resources"};


    @Override
    public void configureProject(final Project project, @NotNull final VirtualFile baseDir, Ref<Module> moduleRef) {
        final RInterpreterService interpreterService = RInterpreterService.getInstance();
        final String interpreterPath = interpreterService.getInterpreterPath();
        if (StringUtil.isEmptyOrSpaces(interpreterPath)) {
            final List<String> homePaths = suggestHomePaths();
            if (!homePaths.isEmpty()) {
                interpreterService.setInterpreterPath(homePaths.get(0));
            }
            ProgressManager.getInstance().run(new Task.Backgroundable(project, "Updating Skeletons", false,
                    PerformInBackgroundOption.ALWAYS_BACKGROUND) {
                @Override
                public void run(@NotNull ProgressIndicator indicator) {
                    RSkeletonGenerator.generateSkeletons(project);
                    VirtualFileManager.getInstance().refreshWithoutFileWatcher(true);
                }
            });
        }
    }


    public List<String> suggestHomePaths() {
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


    private void addInstallationFromPaths(@NotNull final ArrayList<String> result, @NotNull final String scriptName,
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


    public static void addFromPathVariable(Collection<String> result) {
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
}
