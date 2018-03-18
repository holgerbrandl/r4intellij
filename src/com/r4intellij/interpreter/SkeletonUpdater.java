/*
 * Copyright 2011 Holger Brandl
 *
 * This code is licensed under BSD. For details see
 * http://www.opensource.org/licenses/bsd-license.php
 */

package com.r4intellij.interpreter;

import com.google.common.collect.Lists;
import com.intellij.notification.Notification;
import com.intellij.notification.NotificationType;
import com.intellij.notification.Notifications;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.startup.StartupActivity;
import com.r4intellij.packages.RIndexCache;
import com.r4intellij.packages.RSkeletonGenerator;
import com.r4intellij.settings.LibraryUtil;
import com.r4intellij.settings.RSettings;
import org.jetbrains.annotations.NotNull;

import java.util.concurrent.atomic.AtomicBoolean;

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
        LibraryUtil.createLibrary(project, LibraryUtil.R_LIBRARY, Lists.newArrayList(), false);
        LibraryUtil.createLibrary(project, LibraryUtil.R_SKELETONS, Lists.newArrayList(), true);

        if (!RSettings.hasInterpreter()) {
            Notifications.Bus.notify(new Notification("R Language Support",
                    "No R interpreter defined",
                    "Many R related features like completion, code checking and help won't be available. You can set an interpreter under Preferences->Languages->R", NotificationType.ERROR));
        }

        // This code is executed after the project was opened.
        RIndexCache.getInstance();
        // disabled
//        RSkeletonGenerator.updateSkeletons(project, false);
        // in favor of optional thread-safe update in com.r4intellij.psi.references.RResolver.addFromSkeletonsAndRLibrary()

        //
        // Fruitless attempt to update the indices when an R file (actual or via literal injection) is loaded>:
        // Unfortunately, the listeners catch just file events but not literal injections
        //
        // register for file event so that we can attach the r-module as needed to avoid unneeded indexing
        // see discussion on https://intellij-support.jetbrains.com/hc/en-us/community/posts/115000440770-How-to-search-global-library-without-module-attachment?page=1#community_comment_115000362250
//        MessageBusConnection connection = project.getMessageBus().connect();
//        connection.subscribe(FileEditorManagerListener.FILE_EDITOR_MANAGER, new MyFileEditorManagerListener());
//        connection.subscribe(FileEditorManagerListener.Before.FILE_EDITOR_MANAGER, new MyFileEditorManagerBefore());
//        connection.subscribe(FileTypeManager.TOPIC, new MyFileTypeListener(project));
    }


    private static AtomicBoolean didStartupIndexRefresh = new AtomicBoolean(false);


    public static void updateSkeletonsLaterOptional(@NotNull Project project) {

        if (!didStartupIndexRefresh.get()) {
            didStartupIndexRefresh.set(true);
            ApplicationManager.getApplication().invokeLater(() -> {
                System.out.println("updating skeletons...");
                RSkeletonGenerator.updateSkeletons(project, false);
            });

        }
    }

//
// see comments and disabled listeners from above
//
//    private void setupRForModuleLater(@NotNull Project project, Module moduleForFile) {
//        ApplicationManager.getApplication().invokeLater(() -> setupRForModule(project, moduleForFile));
//    }
//
//
//    private void setupRForModule(@NotNull Project project, Module moduleForFile) {
//        // Create additional library for user user code
//        // make sure that library and skeltons are attached to all modules
//
//        LibraryUtil.createLibrary(project, LibraryUtil.R_LIBRARY, Lists.newArrayList(), false);
//        LibraryUtil.createLibrary(project, LibraryUtil.R_SKELETONS, Lists.newArrayList(), true);
//
//
//        // This code is executed after the project was opened.
////        RIndexCache.getInstance();
//
//        // todo let user select refresh strategy via balloon tooltip
////        if (!didStartupIndexRefresh.get()) {
////            didStartupIndexRefresh.set(true);
////            RSkeletonGenerator.updateSkeletons(project, false);
////        }
//    }
//
//
//    private static class MyFileEditorManagerBefore implements FileEditorManagerListener.Before {
//        @Override
//        public void beforeFileOpened(@NotNull FileEditorManager fileEditorManager, @NotNull VirtualFile virtualFile) {
//            System.out.println("foo2");
//        }
//
//
//        @Override
//        public void beforeFileClosed(@NotNull FileEditorManager fileEditorManager, @NotNull VirtualFile virtualFile) {
////                System.out.println("foo2");
//        }
//    }
//
//
//    private class MyFileEditorManagerListener implements FileEditorManagerListener {
//        private MyFileEditorManagerListener() {
//        }
//
//
//        public void fileOpened(@NotNull FileEditorManager source, @NotNull VirtualFile file) {
////            BreadcrumbsInitializingActivity.reinitBreadcrumbsComponent(source, file);
//            String canonicalPath = file.getCanonicalPath();
//
//            if (canonicalPath != null && (canonicalPath.endsWith(".R") || canonicalPath.endsWith(".r"))) {
//                // see com/r4intellij/psi/references/RResolver.java:47
//                ProjectFileIndex fileIndex = ProjectRootManager.getInstance(source.getProject()).getFileIndex();
//                Module moduleForFile = fileIndex.getModuleForFile(file);
//                setupRForModule(source.getProject(), moduleForFile);
//            }
//
////            System.out.println("foo");
//
//        }
//
//
//        @Override
//        public void fileClosed(@NotNull FileEditorManager source, @NotNull VirtualFile file) {
//            System.out.println("foo");
//
//        }
//
//
//        @Override
//        public void selectionChanged(@NotNull FileEditorManagerEvent event) {
//            System.out.println("foo");
//        }
//    }
//
//
//    private static class MyFileTypeListener implements FileTypeListener {
//        private final Project myProject;
//
//
//        public MyFileTypeListener(@NotNull Project project) {
//            this.myProject = project;
//        }
//
//
//        public void fileTypesChanged(@NotNull FileTypeEvent event) {
//            if (!this.myProject.isDisposed()) {
//                System.out.println("foo");
////                BreadcrumbsInitializingActivity.reinitBreadcrumbsInAllEditors(this.myProject);
//            }
//
//        }
//    }
}