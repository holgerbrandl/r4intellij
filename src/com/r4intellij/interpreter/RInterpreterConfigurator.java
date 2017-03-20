package com.r4intellij.interpreter;

import com.intellij.openapi.module.Module;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.Ref;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.platform.DirectoryProjectConfigurator;
import org.jetbrains.annotations.NotNull;

// also see https://android.googlesource.com/platform/tools/idea/+/9c6f3112ffe942e4bb0b5d5d8476ce7014499650/python/ide/src/com/jetbrains/python/PythonSdkConfigurator.java
public class RInterpreterConfigurator implements DirectoryProjectConfigurator {


    @Override
    public void configureProject(final Project project, @NotNull final VirtualFile baseDir, Ref<Module> moduleRef) {
        RSkeletonGenerator.updateSkeletons(project);
        // not needed becaues done internally
        // VirtualFileManager.getInstance().refreshWithoutFileWatcher(true);
    }
}
