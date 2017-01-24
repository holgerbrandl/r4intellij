package com.r4intellij.interpreter;

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
import com.intellij.openapi.progress.ProgressIndicator;
import com.intellij.openapi.progress.ProgressManager;
import com.intellij.openapi.progress.Task;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.io.FileUtil;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.openapi.vfs.VfsUtil;
import com.intellij.openapi.vfs.VirtualFile;
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
import java.util.Collections;
import java.util.Set;

import static com.r4intellij.interpreter.RInterpreterConfigurable.R_SKELETONS;
import static com.r4intellij.interpreter.RInterpreterConfigurable.createLibrary;


public class RSkeletonGenerator {
    protected static final Logger LOG = Logger.getInstance("#" + RSkeletonGenerator.class.getName());

    private static final String R_HELPER_SKELETONIZE_PACKAGE = "skeletonize_package.R";

    public static final String SKELETON_DIR_NAME = "r_skeletons";


    // entry point for configurable interface and action
    public static void generateSkeletons(@NotNull final Project project) {

        final Application application = ApplicationManager.getApplication();

        application.invokeLater(new Runnable() {
            @Override
            public void run() {

                application.runWriteAction(new Runnable() {
                    @Override
                    public void run() {
                        // add all paths to library
                        final String skeletonLibraryPath = RSkeletonGenerator.getSkeletonsPath();

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
                        createLibrary(R_SKELETONS, Collections.singletonList(skeletonLibraryPath), project);

                        //TODO still needed?
//                        final String userSkeletonsPath = RHelpersLocator.getHelperPath("r-user-skeletons");
//                        createLibrary(RInterpreterConfigurable.The_R_USER_SKELETONS, userSkeletonsPath, project);
                    }
                });

                // now do the actual work
                generateSmartSkeletons(project);
            }
        });
    }


    private static void generateSmartSkeletons(@NotNull final Project project) {
        // http://stackoverflow.com/questions/18725340/create-a-background-task-in-intellij-plugin

        ProgressManager.getInstance().run(new Task.Backgroundable(project, "Updating Skeletons", false) {
            @Override
            public void run(@NotNull ProgressIndicator indicator) {
                ApplicationManager.getApplication().executeOnPooledThread(new Runnable() {
                    @Override
                    public void run() {
                        RSkeletonGenerator.runSkeletonGeneration(indicator);

                        PsiDocumentManager.getInstance(project).commitAllDocuments();

                        String path = RSkeletonGenerator.getSkeletonsPath();

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
                LOG.info("start processing " + file.getPath());
                PsiFile psiFile = PsiManager.getInstance(project).findFile(file);
                assert psiFile != null;
                psiFile.acceptChildren(new TypedFunctionVisitor(packageDocument, file, project, packageName));
            }

        } catch (IOException e) {
            LOG.error(e);
        }
    }


    public static String getSkeletonsPath() {
        final String basePath = PathManager.getSystemPath();

        // todo this should include the interpreter version as well
        String interpreterPath = RInterpreterService.getInstance().getInterpreterPath();
        int interpreterHash = FileUtil.toSystemIndependentName(interpreterPath).hashCode();

        return (basePath + File.separator + SKELETON_DIR_NAME) + File.separator + Math.abs(interpreterHash) + File.separator;
    }


    private static void runSkeletonGeneration(@Nullable ProgressIndicator indicator) {
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

            // skip existing skeletons
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

}
