package com.r4intellij.interpreter;

import com.google.common.collect.Ordering;
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
import java.nio.file.Files;
import java.nio.file.StandardCopyOption;
import java.util.*;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;

import static com.r4intellij.packages.LocalRUtil.DEFAULT_PACKAGES;
import static com.r4intellij.packages.RHelperUtil.PluginResourceFile;
import static com.r4intellij.packages.RHelperUtil.RRunResult;
import static com.r4intellij.settings.LibraryUtil.R_SKELETONS;
import static com.r4intellij.settings.LibraryUtil.createLibrary;


public class RSkeletonGenerator {
    protected static final Logger LOG = Logger.getInstance("#" + RSkeletonGenerator.class.getName());

    public static final PluginResourceFile SKELETONIZE_PACKAGE = new PluginResourceFile("skeletonize_package.R");

    public static final String SKELETON_DIR_NAME = "r_skeletons";


    // entry point for configurable interface and action
    public static void updateSkeletons(@NotNull final Project project) {

        final Application application = ApplicationManager.getApplication();

        // first (if not yet present create a library within the project that will contain the package skeletons
        application.runWriteAction(() -> {
            // add all paths to library
            final String skeletonLibraryPath = RSkeletonGenerator.getSkeletonsPath();

            File skeletonLibDir = new File(skeletonLibraryPath);
            if (!skeletonLibDir.exists()) {
                if (!skeletonLibDir.mkdir()) {
                    LOG.warn("Failed to create skeleton dir");
                }
            }

            createLibrary(R_SKELETONS, Collections.singletonList(skeletonLibraryPath), project);
        });

        // now do the actual work
        generateSmartSkeletons(project);
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
                    }
                });
            }
        });
    }


    public static String getSkeletonsPath() {
        final String basePath = PathManager.getSystemPath();

        // todo this should include the interpreter version as well
        String interpreterPath = RSettings.getInstance().getInterpreterPath();
        int interpreterHash = FileUtil.toSystemIndependentName(interpreterPath).hashCode();

        return (basePath + File.separator + SKELETON_DIR_NAME) + File.separator + Math.abs(interpreterHash) + File.separator;
    }


    // extracted from https://support.rstudio.com/hc/en-us/articles/201057987-Quick-list-of-useful-R-packages
    public static final List<String> COMMON_PACKAGES = Arrays.asList("RSQLite", "xlsx", "foreign", "dplyr", "tidyr",
            "stringr", "lubridate", "ggplot2", "ggvis", "rgl", "htmlwidgets", "googleVis", "car", "mgcv", "nlme",
            "randomForest", "multcomp", "vcd", "glmnet", "elastic", "survival", "caret", "shiny", "non", "xtable",
            "maptools", "maps", "ggmap", "zoo", "xts", "quantmod", "Rcpp", "table", "parallel", "XML", "jsonlite",
            "httr", "devtools", "testthat", "roxygen2");


    private static Integer getIndexPriority(RPackage r) {
        if (DEFAULT_PACKAGES.contains(r.getName())) return 2;
        if (COMMON_PACKAGES.contains(r.getName())) return 1;
        return 0;
    }


    private static void updateSkeletons(@Nullable ProgressIndicator indicator) {
        final String interpreter = RSettings.getInstance().getInterpreterPath();

        if (StringUtil.isEmptyOrSpaces(interpreter)) return;


        int processed = 0;

        Collection<RPackage> packages = RPackageService.getInstance().getPackages();

        // resort them so that the most popular ones are indexed first
        packages = Ordering.natural().reverse().onResultOf(RSkeletonGenerator::getIndexPriority)
                .sortedCopy(packages).stream().collect(Collectors.toList());


        // additional threading here kills the computer
        ExecutorService es = Executors.newFixedThreadPool(4);


        for (RPackage rPackage : packages) {

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
            String pckgName = rPackage.getName();
            File skeletonFile = new File(skeletonsDir, pckgName + ".R");
            if (isValidSkeleton(skeletonFile)) {
                continue;
            }


            es.submit(() -> {
                try {

//            ApplicationManager.getApplication().executeOnPooledThread(new Runnable() {
//
//                @Override
//                public void run() {

                    LOG.info("building skeleton for " + pckgName);


                    // build the skeletons in tmp and move them once done so avoid incomplete file index failures
                    File tempSkeleton = Files.createTempFile("r4j_skel_" + pckgName + "_", ".R").toFile();

                    RRunResult output = RHelperUtil.runHelperWithArgs(SKELETONIZE_PACKAGE, pckgName, tempSkeleton.getAbsolutePath());

                    if (output != null && output.getExitCode() != 0) {
                        LOG.error("Failed to generate skeleton for '" + pckgName + "'. Exit code: " + output.getExitCode());
                        LOG.error(output.getStdErr());
                    } else if (isValidSkeleton(tempSkeleton)) {
                        Files.move(tempSkeleton.toPath(), skeletonFile.toPath(), StandardCopyOption.REPLACE_EXISTING);
                    } else {
                        LOG.error("Failed to generate skeleton for '" + pckgName + "'");
                    }

                } catch (IOException e) {
                    LOG.error("Failed to generate skeleton for '" + pckgName + "' due to io issue", e);

                }
            });
        }

        // wait until all skeletons are built
        try {
            es.shutdown();
            es.awaitTermination(1, TimeUnit.HOURS);
        } catch (InterruptedException e) {
            e.printStackTrace();
        }

    }


    public static boolean isValidSkeleton(File skeletonFile) {
        return skeletonFile.exists() && isComplete(skeletonFile) && isCurrentVersion(skeletonFile);
    }


    /**
     * Scan for final EOF tag in skeleton file.
     */
    private static boolean isComplete(File skeletonFile) {
        boolean hasEOF = false;
        try {
            Scanner scanner = new Scanner(skeletonFile);

            while (scanner.hasNextLine()) {
                String curLine = scanner.nextLine().trim();
                if (!curLine.isEmpty()) hasEOF = curLine.equals("## EOF");
            }
        } catch (FileNotFoundException e) {
            return false;
        }

        return hasEOF;
    }


    // note: just change in sync with ./r-helpers/skeletonize_package.R
    public static final int SKELETONIZE_VERSION = 4;


    private static boolean isCurrentVersion(File skeletonFile) {

        // TODO why don't we use the stub here (should be faster)


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


    // note keep since we want to bring back the type system some day
    private static void generateTyopedSkeletonForPackage(@NotNull final VirtualFile packageDir, @NotNull final Project project) {
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
}
