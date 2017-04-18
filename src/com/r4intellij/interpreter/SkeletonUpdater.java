/*
 * Copyright 2011 Holger Brandl
 *
 * This code is licensed under BSD. For details see
 * http://www.opensource.org/licenses/bsd-license.php
 */

package com.r4intellij.interpreter;

import com.google.common.collect.Lists;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.startup.StartupActivity;
import com.r4intellij.packages.RIndexCache;
import com.r4intellij.packages.RSkeletonGenerator;
import com.r4intellij.settings.LibraryUtil;
import org.jetbrains.annotations.NotNull;

/**
 * @author Holger Brandl
 */
// from https://www.cqse.eu/en/blog/intellij-plugin-tutorial/
public class SkeletonUpdater implements StartupActivity {

    @Override
    public void runActivity(@NotNull Project project) {

        // Create additional library for user user code
        // make sure that library and skeltons are attached to all modules
//         TODO move into post-project-create section to prevent recreation of module on startup
        LibraryUtil.createLibrary(LibraryUtil.R_LIBRARY, Lists.newArrayList(), project, false);
        LibraryUtil.createLibrary(LibraryUtil.R_SKELETONS, Lists.newArrayList(), project, true);

        // This code is executed after the project was opened.
        RIndexCache.getInstance();
        RSkeletonGenerator.updateSkeletons(project);
    }
}