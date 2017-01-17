package com.r4intellij.interpreter;

import com.google.common.collect.Lists;
import com.intellij.execution.process.CapturingProcessHandler;
import com.intellij.execution.process.ProcessOutput;
import com.intellij.openapi.application.Application;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.application.PathManager;
import com.intellij.openapi.diagnostic.Logger;
import com.intellij.openapi.module.Module;
import com.intellij.openapi.module.ModuleManager;
import com.intellij.openapi.progress.ProgressIndicator;
import com.intellij.openapi.progress.ProgressManager;
import com.intellij.openapi.progress.Task;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.roots.ModifiableModelsProvider;
import com.intellij.openapi.roots.ModifiableRootModel;
import com.intellij.openapi.roots.OrderRootType;
import com.intellij.openapi.roots.libraries.Library;
import com.intellij.openapi.roots.libraries.LibraryTable;
import com.intellij.openapi.util.io.FileUtil;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.openapi.vfs.LocalFileSystem;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.openapi.vfs.VirtualFileManager;
import com.r4intellij.RHelpersLocator;
import org.jetbrains.annotations.NotNull;

import java.io.File;
import java.io.IOException;
import java.util.List;


public class RSkeletonGenerator {
    protected static final Logger LOG = Logger.getInstance("#" + RSkeletonGenerator.class.getName());

    protected static final String R_GENERATOR = "r-generator.r";

    public static final String SKELETON_DIR_NAME = "r_skeletons";

    protected static final int MINUTE = 60 * 1000;


    public static String getSkeletonsPath(@NotNull final String interpreterHome) {
        final String basePath = PathManager.getSystemPath();

        // todo this should include the interpreter version as well
        int interpreterHash = FileUtil.toSystemIndependentName(interpreterHome).hashCode();

        return getSkeletonsRootPath(basePath) + File.separator + Math.abs(interpreterHash) + File.separator;
    }


    public static String getSkeletonsRootPath(@NotNull final String basePath) {
        return basePath + File.separator + SKELETON_DIR_NAME;
    }


    public static void runSkeletonGeneration() {
        final String path = RInterpreterService.getInstance().getInterpreterPath();
        if (StringUtil.isEmptyOrSpaces(path)) return;
        final String helperPath = RHelpersLocator.getHelperPath(R_GENERATOR);
        try {
            final String skeletonsPath = getSkeletonsPath(path);
            final File skeletonsDir = new File(skeletonsPath);
            if (!skeletonsDir.exists() && !skeletonsDir.mkdirs()) {
                LOG.error("Can't create skeleton dir " + String.valueOf(skeletonsPath));
            }
            final String commandLine = path + " --slave -f " + helperPath + " --args " + skeletonsPath;
            final Process process = Runtime.getRuntime().exec(commandLine);
            final CapturingProcessHandler processHandler = new CapturingProcessHandler(process, null, commandLine);
            final ProcessOutput output = processHandler.runProcess(MINUTE * 5);
            if (output.getExitCode() != 0) {
                LOG.error("Failed to generate skeletons. Exit code: " + output.getExitCode());
                LOG.error(output.getStderrLines());
            }
        } catch (IOException e) {
            LOG.error(e);
        }
    }


    public static void generateSkeletons(@NotNull final Project project) {

        final Application application = ApplicationManager.getApplication();

        application.invokeLater(new Runnable() {
            @Override
            public void run() {

                application.runWriteAction(new Runnable() {
                    @Override
                    public void run() {
                        // add all paths to library
                        final String skeletonLibraryPath =
                                RSkeletonGenerator.getSkeletonsPath(RInterpreterService.getInstance().getInterpreterPath());
                        File skeletonLibDir = new File(skeletonLibraryPath);
                        if (!skeletonLibDir.exists()) {
                            if (!skeletonLibDir.mkdir()) {
                                LOG.warn("Failed to create skeleton dir");
                            }
                        }
                        String skeletons = RHelpersLocator.getHelperPath("r-skeletons");
                        File pregeneratedSkeletonsDir = new File(skeletons);
                        if (!pregeneratedSkeletonsDir.exists()) {
                            LOG.info("Pre-generated skeletons not found");
                        } else {
                            try {
                                FileUtil.copyDirContent(pregeneratedSkeletonsDir, skeletonLibDir);
                            } catch (IOException e) {
                                LOG.error(e);
                            }
                        }
                        generateLibrary(RInterpreterConfigurable.R_SKELETONS, skeletonLibraryPath, project);
                        final String userSkeletonsPath = RHelpersLocator.getHelperPath("r-user-skeletons");
                        generateLibrary(RInterpreterConfigurable.The_R_USER_SKELETONS, userSkeletonsPath, project);
                    }
                });

                ProgressManager.getInstance().run(new Task.Backgroundable(project, "Updating Skeletons", false) {
                    @Override
                    public void run(@NotNull ProgressIndicator indicator) {
                        VirtualFileManager.getInstance().refreshWithoutFileWatcher(true);
                        //TODO: run my brand new action
                        RSkeletonGenerator.runSkeletonGeneration();
                        VirtualFileManager.getInstance().refreshWithoutFileWatcher(true);
                    }
                });
            }
        });
    }


    private static void generateLibrary(final String name, final String path, @NotNull final Project project) {
        final ModifiableModelsProvider modelsProvider = ModifiableModelsProvider.SERVICE.getInstance();
        final LibraryTable.ModifiableModel model = modelsProvider.getLibraryTableModifiableModel(project);
        Library library = model.getLibraryByName(name);
        if (library == null) {
            library = model.createLibrary(name);
        }
        fillLibrary(library, Lists.newArrayList(path));
        model.commit();
        final Library.ModifiableModel libModel = library.getModifiableModel();
        libModel.commit();
        final Module[] modules = ModuleManager.getInstance(project).getModules();
        for (Module module : modules) {
            final ModifiableRootModel modifiableModel = modelsProvider.getModuleModifiableModel(module);
            modifiableModel.addLibraryEntry(library);
            modelsProvider.commitModuleModifiableModel(modifiableModel);
        }
    }


    public static void fillLibrary(@NotNull final Library lib, @NotNull final List<String> paths) {
        Library.ModifiableModel modifiableModel = lib.getModifiableModel();
        for (String root : lib.getUrls(OrderRootType.CLASSES)) {
            modifiableModel.removeRoot(root, OrderRootType.CLASSES);
        }
        for (String dir : paths) {
            final VirtualFile pathEntry = LocalFileSystem.getInstance().findFileByPath(dir);
            if (pathEntry != null) {
                modifiableModel.addRoot(pathEntry, OrderRootType.CLASSES);
            } else {
                modifiableModel.addRoot("file://" + dir, OrderRootType.CLASSES);
            }
        }
        modifiableModel.commit();
    }
}
