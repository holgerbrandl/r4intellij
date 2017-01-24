package com.r4intellij.interpreter;

import com.google.common.collect.Lists;
import com.intellij.execution.ExecutionException;
import com.intellij.execution.configurations.GeneralCommandLine;
import com.intellij.execution.process.CapturingProcessHandler;
import com.intellij.execution.process.ProcessOutput;
import com.intellij.openapi.application.Application;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.application.PathManager;
import com.intellij.openapi.diagnostic.Logger;
import com.intellij.openapi.editor.Document;
import com.intellij.openapi.fileEditor.FileDocumentManager;
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
import com.intellij.openapi.vfs.VfsUtil;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.openapi.vfs.VirtualFileManager;
import com.intellij.psi.PsiDocumentManager;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiManager;
import com.intellij.util.DocumentUtil;
import com.r4intellij.RHelpersLocator;
import com.r4intellij.RPsiUtils;
import com.r4intellij.packages.RPackage;
import com.r4intellij.packages.RPackageService;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.io.File;
import java.io.IOException;
import java.util.List;
import java.util.Set;


public class RSkeletonGenerator {
    protected static final Logger LOG = Logger.getInstance("#" + RSkeletonGenerator.class.getName());

    protected static final String R_HELPER_SKELETONIZE_PACKAGE = "skeletonize_package.R";

    public static final String SKELETON_DIR_NAME = "r_skeletons";

    protected static final int MINUTE = 60 * 1000;


    public static void generateSmartSkeletons(@NotNull final Project project) {
        // http://stackoverflow.com/questions/18725340/create-a-background-task-in-intellij-plugin

        ProgressManager.getInstance().run(new Task.Backgroundable(project, "Updating Skeletons", false) {
            @Override
            public void run(@NotNull ProgressIndicator indicator) {
                ApplicationManager.getApplication().executeOnPooledThread(new Runnable() {
                    @Override
                    public void run() {
                        RSkeletonGenerator.runSkeletonGeneration(indicator);

                        PsiDocumentManager.getInstance(project).commitAllDocuments();
                        final String path = RSkeletonGenerator.getSkeletonsPath();

                        VirtualFile skeletonDir = VfsUtil.findFileByIoFile(new File(path), true);
                        if (skeletonDir == null) {
                            LOG.info("Failed to locate skeletons directory");
                            return;
                        }

                        int processed = 0;
                        for (final VirtualFile packageDir : skeletonDir.getChildren()) {

                            if (packageDir.isDirectory()) {
                                continue;
                            }
//                            ApplicationManager.getApplication().invokeLater(
                            ApplicationManager.getApplication().executeOnPooledThread(new Runnable() {
                                @Override
                                public void run() {
                                    ApplicationManager.getApplication().runWriteAction(new Runnable() {
                                        @Override

                                        public void run() {
                                            indicator.setFraction((double) processed / skeletonDir.getChildren().length);
                                            indicator.setText("Indexing " + packageDir.getName());

                                            generateSkeletonsForPackage(packageDir, project);
                                            try {
                                                packageDir.delete(this);
                                            } catch (IOException e) {
                                                LOG.error("Failed to delete " + packageDir.getPath());
                                            }
                                        }
                                    });
                                }
                            });
                        }
                    }
                });
            }
        });
    }


    private static void generateSkeletonsForPackage(@NotNull final VirtualFile packageDir, @NotNull final Project project) {
        String packageName = packageDir.getName();
        //TODO: DELETE THIS CHECK!!! it is here only for speeding checks while developing
//        if (!packageName.equals("base") && !packageName.equals("codetools")) {
//            return;
//        }
        VirtualFile skeletonsDir = packageDir.getParent();
        try {
            VirtualFile packageFile = skeletonsDir.findOrCreateChildData(project, packageName + ".r");
            final Document packageDocument = FileDocumentManager.getInstance().getDocument(packageFile);
            assert packageDocument != null;
            DocumentUtil.writeInRunUndoTransparentAction(new Runnable() {
                @Override
                public void run() {
                    packageDocument.deleteString(0, packageDocument.getTextLength());
                }
            });
            for (final VirtualFile file : packageDir.getChildren()) {
                generateSkeletonsForFile(file, packageDocument, project, packageName);
            }
        } catch (IOException e) {
            LOG.error(e);
        }
    }


    private static void generateSkeletonsForFile(@NotNull final VirtualFile file,
                                                 @NotNull final Document packageDocument,
                                                 @NotNull final Project project,
                                                 String packageName) {
        LOG.info("start processing " + file.getPath());
        PsiFile psiFile = PsiManager.getInstance(project).findFile(file);
        assert psiFile != null;
        psiFile.acceptChildren(new TypedFunctionVisitor(packageDocument, file, project, packageName));
    }





    public static String getSkeletonsPath() {
        final String basePath = PathManager.getSystemPath();

        // todo this should include the interpreter version as well
        String interpreterPath = RInterpreterService.getInstance().getInterpreterPath();
        int interpreterHash = FileUtil.toSystemIndependentName(interpreterPath).hashCode();

        return getSkeletonsRootPath(basePath) + File.separator + Math.abs(interpreterHash) + File.separator;
    }


    public static String getSkeletonsRootPath(@NotNull final String basePath) {
        return basePath + File.separator + SKELETON_DIR_NAME;
    }


    public static void runSkeletonGeneration(@Nullable ProgressIndicator indicator) {
        final String interpreter = RInterpreterService.getInstance().getInterpreterPath();

        if (StringUtil.isEmptyOrSpaces(interpreter)) return;


        int processed = 0;

        Set<RPackage> packages = RPackageService.getInstance().getPackages();
        for (RPackage rPackage : packages) {
            LOG.info("building skeleton for " + rPackage.getName());

            if (indicator != null) {
                indicator.setFraction((double) processed++ / (2 * packages.size()));
                indicator.setText2("Indexing " + rPackage);
            }

            final String helperPath = RHelpersLocator.getHelperPath(R_HELPER_SKELETONIZE_PACKAGE);

            final String skeletonsPath = getSkeletonsPath();
            final File skeletonsDir = new File(skeletonsPath);

            if (!skeletonsDir.exists() && !skeletonsDir.mkdirs()) {
                LOG.error("Can't create skeleton dir " + String.valueOf(skeletonsPath));
            }

            if (skeletonsDir.exists() && skeletonsDir.listFiles().length > 0) continue;

            try {
                GeneralCommandLine gcl = new GeneralCommandLine().
                        withExePath(interpreter).
                        withParameters("--slave", "-f", helperPath, "--args", skeletonsPath, rPackage.getName());

                final CapturingProcessHandler processHandler = new CapturingProcessHandler(gcl);
                final ProcessOutput output = processHandler.runProcess(5 * RPsiUtils.MINUTE);

                if (output.getExitCode() != 0) {
                    LOG.error("Failed to generate skeleton for '" + rPackage.getName() + "'. Exit code: " + output.getExitCode());
                    LOG.error(output.getStderrLines());
                }
            } catch (ExecutionException e) {
                LOG.error(e);
            }
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
                                RSkeletonGenerator.getSkeletonsPath();
                        File skeletonLibDir = new File(skeletonLibraryPath);
                        if (!skeletonLibDir.exists()) {
                            if (!skeletonLibDir.mkdir()) {
                                LOG.warn("Failed to create skeleton dir");
                            }
                        }
//                        String skeletons = RHelpersLocator.getHelperPath("r-skeletons");
//                        File pregeneratedSkeletonsDir = new File(skeletons);
//                        if (!pregeneratedSkeletonsDir.exists()) {
//                            LOG.info("Pre-generated skeletons not found");
//                        } else {
//                            try {
//                                FileUtil.copyDirContent(pregeneratedSkeletonsDir, skeletonLibDir);
//                            } catch (IOException e) {
//                                LOG.error(e);
//                            }
//                        }
                        generateLibrary(RInterpreterConfigurable.R_SKELETONS, skeletonLibraryPath, project);

                        //TODO still needed?
//                        final String userSkeletonsPath = RHelpersLocator.getHelperPath("r-user-skeletons");
//                        generateLibrary(RInterpreterConfigurable.The_R_USER_SKELETONS, userSkeletonsPath, project);
                    }
                });

                ProgressManager.getInstance().run(new Task.Backgroundable(project, "Updating Skeletons", false) {
                    @Override
                    public void run(@NotNull ProgressIndicator indicator) {
                        VirtualFileManager.getInstance().refreshWithoutFileWatcher(true);
                        //TODO: run my brand new action
                        RSkeletonGenerator.runSkeletonGeneration(indicator);
                        VirtualFileManager.getInstance().refreshWithoutFileWatcher(true);
                    }
                });
            }
        });
    }


    private static void generateLibrary(final String name, final String path, @NotNull final Project project) {
        ModifiableModelsProvider modelsProvider = ModifiableModelsProvider.SERVICE.getInstance();
        LibraryTable.ModifiableModel model = modelsProvider.getLibraryTableModifiableModel(project);
        Library library = model.getLibraryByName(name);

        if (library == null) {
            library = model.createLibrary(name);
        }

        fillLibrary(library, Lists.newArrayList(path));
        model.commit();

        Library.ModifiableModel libModel = library.getModifiableModel();
        libModel.commit();

        Module[] modules = ModuleManager.getInstance(project).getModules();
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
