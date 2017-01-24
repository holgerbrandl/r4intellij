package com.r4intellij.interpreter;

import com.intellij.openapi.module.Module;
import com.intellij.openapi.progress.PerformInBackgroundOption;
import com.intellij.openapi.progress.ProgressIndicator;
import com.intellij.openapi.progress.ProgressManager;
import com.intellij.openapi.progress.Task;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.Ref;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.openapi.vfs.VirtualFileManager;
import com.intellij.platform.DirectoryProjectConfigurator;
import org.jetbrains.annotations.NotNull;

public class RInterpreterConfigurator implements DirectoryProjectConfigurator {


    @Override
    public void configureProject(final Project project, @NotNull final VirtualFile baseDir, Ref<Module> moduleRef) {
        final RInterpreterService interpreterService = RInterpreterService.getInstance();
        final String interpreterPath = interpreterService.getInterpreterPath();

        if (StringUtil.isEmptyOrSpaces(interpreterPath
        )) {

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
}
