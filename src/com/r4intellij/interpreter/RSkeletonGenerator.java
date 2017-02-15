package com.r4intellij.interpreter;

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
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiManager;
import com.intellij.util.DocumentUtil;
import com.r4intellij.packages.RHelperUtil;
import com.r4intellij.packages.RPackage;
import com.r4intellij.packages.RPackageService;
import com.r4intellij.settings.RSettings;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.Collections;
import java.util.Scanner;
import java.util.Set;

import static com.r4intellij.packages.RHelperUtil.PluginResourceFile;
import static com.r4intellij.packages.RHelperUtil.RRunResult;
import static com.r4intellij.settings.LibraryUtil.R_SKELETONS;
import static com.r4intellij.settings.LibraryUtil.createLibrary;


public class RSkeletonGenerator {
    protected static final Logger LOG = Logger.getInstance("#" + RSkeletonGenerator.class.getName());

    private static final PluginResourceFile SKELETONIZE_PACKAGE = new PluginResourceFile("skeletonize_package.R");

    public static final String SKELETON_DIR_NAME = "r_skeletons";


    // entry point for configurable interface and action
    public static void generateSkeletons(@NotNull final Project project) {

        final Application application = ApplicationManager.getApplication();

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
//                        createLibrary(RSettingsConfigurable.USER_SKELETONS, userSkeletonsPath, project);
            }
        });

        // now do the actual work
        generateSmartSkeletons(project);
        ;
    }


    private static void generateSmartSkeletons(@NotNull final Project project) {
        // http://stackoverflow.com/questions/18725340/create-a-background-task-in-intellij-plugin

        ProgressManager.getInstance().run(new Task.Backgroundable(project, "Updating Skeletons", false) {
            @Override
            public void run(@NotNull ProgressIndicator indicator) {
                ApplicationManager.getApplication().executeOnPooledThread(new Runnable() {
                    @Override
                    public void run() {
                        RSkeletonGenerator.updateSkeletons(indicator);

                        // TBD seems to cause thread exeception. Needed?
//                        PsiDocumentManager.getInstance(project).commitAllDocuments();

                        String path = RSkeletonGenerator.getSkeletonsPath();

                        VirtualFile skeletonDir = VfsUtil.findFileByIoFile(new File(path), true);

                        if (skeletonDir == null) {
                            LOG.info("Failed to locate skeletons directory");
                            return;
                        }

//                        int processed = 0;
//                        for (final VirtualFile packageDir : skeletonDir.getChildren()) {
//
//                            if (!packageDir.isDirectory()) {
//                                continue;
//                            }
//
//                            ApplicationManager.getApplication().invokeLater(new Runnable() {
////                            ApplicationManager.getApplication().executeOnPooledThread(new Runnable() {
//                                @Override
//                                public void run() {
//                                    ApplicationManager.getApplication().runWriteAction(new Runnable() {
//                                        @Override
//
//                                        public void run() {
//                                            indicator.setFraction((double) processed / skeletonDir.getChildren().length);
//                                            indicator.setText("Indexing " + packageDir.getName());
//
//                                            generateSkeletonsForPackage(packageDir, project);
//                                            try {
//                                                packageDir.delete(this);
//                                            } catch (IOException e) {
//                                                LOG.error("Failed to delete " + packageDir.getPath());
//                                            }
//                                        }
//                                    });
//                                }
//                            });
//                        }
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

                // todo reenable typing
                psiFile.acceptChildren(new SimpleFunctionVisitor(packageDocument, file, project, packageName));
//                psiFile.acceptChildren(new TypedFunctionVisitor(packageDocument, file, project, packageName));
            }

        } catch (IOException e) {
            LOG.error(e);
        }
    }


    public static String getSkeletonsPath() {
        final String basePath = PathManager.getSystemPath();

        // todo this should include the interpreter version as well
        String interpreterPath = RSettings.getInstance().getInterpreterPath();
        int interpreterHash = FileUtil.toSystemIndependentName(interpreterPath).hashCode();

        return (basePath + File.separator + SKELETON_DIR_NAME) + File.separator + Math.abs(interpreterHash) + File.separator;
    }


    private static void updateSkeletons(@Nullable ProgressIndicator indicator) {
        final String interpreter = RSettings.getInstance().getInterpreterPath();

        if (StringUtil.isEmptyOrSpaces(interpreter)) return;


        int processed = 0;

        Set<RPackage> packages = RPackageService.getInstance().getPackages();
        for (RPackage rPackage : packages) {
            LOG.info("building skeleton for " + rPackage.getName());

            if (indicator != null) {
                indicator.setFraction((double) processed++ / (2 * packages.size()));
                indicator.setText2("Indexing " + rPackage);
            }

            final String skeletonsPath = getSkeletonsPath();
            final File skeletonsDir = new File(skeletonsPath);

            if (!skeletonsDir.exists() && !skeletonsDir.mkdirs()) {
                LOG.error("Can't create skeleton dir " + String.valueOf(skeletonsPath));
            }

            // skip if skeleton exists already and it is not outdated
            File skeletonFile = new File(skeletonsDir, rPackage.getName() + ".r");
            if (skeletonFile.exists() && isCurrentVersion(skeletonFile)) {
                continue;
            }


//            // skip existing skeleton dir exists already
//            File packageSkelDir = new File(skeletonsDir, rPackage.getName());
//            //noinspection ConstantConditions
//            if (packageSkelDir.exists() && packageSkelDir.listFiles().length > 0) {
//                continue;
//            }
            ApplicationManager.getApplication().executeOnPooledThread(new Runnable() {

                @Override
                public void run() {
                    RRunResult output = RHelperUtil.runHelperWithArgs(SKELETONIZE_PACKAGE, skeletonsPath, rPackage.getName());
                    if (output != null && output.getExitCode() != 0) {
                        LOG.error("Failed to generate skeleton for '" + rPackage.getName() + "'. Exit code: " + output.getExitCode());
                        LOG.error(output.getStdErr());
                    }
                }
            });
        }
    }


    // note: just change in sync with ./r-helpers/skeletonize_package.R
    public static final int SKELETONIZE_VERSION = 1;


    //todo refac to use to-be-impl Rpackage API here
    private static boolean isCurrentVersion(File skeletonFile) {

        // todo tbd why don't we use the stub here (should be faster)
//        VirtualFile virtualFile = VfsTestUtil.findFileByCaseSensitivePath(skeletonFile.getAbsolutePath());
//        final PsiFile psiFile = PsiManager.getInstance(project).findFile(virtualFile);
//
//        Integer fileVersion = PsiTreeUtil.findChildrenOfType(psiFile, RAssignmentStatement.class).stream()
//                .filter(rassign -> Objects.equals(rassign.getName(), ".skeleton_version"))
//                .map(rassign -> Integer.valueOf(rassign.getAssignedValue().getText()))
//                .findFirst().orElse(-1);


        // use basic file search because psi-read access is not allowed from here (and it's also faster)
        Integer fileVersion = -1;
        try {
            Scanner scanner = new Scanner(skeletonFile);
            while (scanner.hasNextLine()) {
                String curLine = scanner.nextLine();
                if (curLine.startsWith(".skeleton_version")) {
                    fileVersion = Integer.valueOf(curLine.split(" = ")[1]);
                    break;
                }
            }
        } catch (FileNotFoundException | ArrayIndexOutOfBoundsException ignored) {
        }

        return SKELETONIZE_VERSION == fileVersion;
    }

}
