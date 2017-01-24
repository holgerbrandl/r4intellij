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
import com.intellij.psi.*;
import com.intellij.util.DocumentUtil;
import com.r4intellij.*;
import com.r4intellij.packages.RPackage;
import com.r4intellij.packages.RPackageService;
import com.r4intellij.psi.RRecursiveElementVisitor;
import com.r4intellij.psi.api.*;
import com.r4intellij.typing.MatchingException;
import com.r4intellij.typing.RSkeletonGeneratorHelper;
import com.r4intellij.typing.RTypeChecker;
import com.r4intellij.typing.types.RType;
import com.r4intellij.typing.types.RUnknownType;
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
        psiFile.acceptChildren(new FunctionVisitor(packageDocument, file, project, packageName));
    }


    static class FunctionVisitor extends RVisitor {
        private final Document myPackageDocument;
        private final VirtualFile myFile;
        private final String myPackageName;
        private Project myProject;


        public FunctionVisitor(Document packageDocument, VirtualFile file, Project project, String packageName) {
            myPackageDocument = packageDocument;
            myFile = file;
            myProject = project;
            myPackageName = packageName;
        }


        @Override
        public void visitAssignmentStatement(@NotNull final RAssignmentStatement o) {
            RPsiElement assignedValue = o.getAssignedValue();
            PsiElement assignee = o.getAssignee();

            //we need this check because of functions like "!" <- ...
            //TODO: delete this check
            if (assignee == null) {
                PsiElement[] children = o.getChildren();
                if (children.length == 0) {
                    return;
                }
                assignee = children[0];
            }
            //TODO: check if we have user skeleton for this function
            if (assignedValue instanceof RFunctionExpression) {
                if (assignee.getText().equals("all")) {
                    System.out.println();
                }
//                String helpText = RPsiUtils.getHelpForFunction(assignee.getText(), myPackageName);
                // todo uncomment once skeletoniziation is more stable and speedy
//                if (helpText != null) {
//                    RHelp help = new RHelp(helpText);
//
//                    final Map<RParameter, RType> parsedTypes = RSkeletonGeneratorHelper.guessArgsTypeFromHelp(help,
//                            (RFunctionExpression) assignedValue);
//
//                    if (parsedTypes != null && !parsedTypes.isEmpty()) {
//                        String tempFileName = myFile.getNameWithoutExtension() + "temp.r";
//                        try {
//                            VirtualFile tempFile = myFile.getParent().findOrCreateChildData(this, tempFileName);
//                            final Document tempDocument = FileDocumentManager.getInstance().getDocument(tempFile);
//                            if (tempDocument != null) {
//                                final List<String> annotations = new ArrayList<String>();
//                                for (Map.Entry<RParameter, RType> entry : parsedTypes.entrySet()) {
//                                    String typeAnnotation = DocStringUtil.generateTypeAnnotation(entry.getKey(), entry.getValue());
//                                    RUtils.appendToDocument(tempDocument, typeAnnotation);
//                                    annotations.add(typeAnnotation);
//                                }
//                                RUtils.appendToDocument(tempDocument, o.getText() + "\n");
//
//                                if (help.myExamples != null && !help.myExamples.isEmpty()) {
//                                    RUtils.appendToDocument(tempDocument, "\n" + help.myExamples + "\n");
//                                }
//
//                                if (help.myUsage != null && !help.myUsage.isEmpty()) {
//                                    RUtils.appendToDocument(tempDocument, "\n" + help.myUsage + "\n");
//                                }
//                                RUtils.saveDocument(tempDocument);
//                                PsiDocumentManager.getInstance(myProject).commitDocument(tempDocument);
//                                Visitor visitor = new Visitor();
//                                FileContentUtil.reparseFiles(tempFile);
//                                PsiFile psiFile = PsiManager.getInstance(myProject).findFile(tempFile);
//                                if (psiFile != null && psiFile.isValid()) {
//                                    psiFile.acceptChildren(visitor);
//                                }
//                                if (!visitor.hasErrors()) {
//                                    RUtils.appendToDocument(myPackageDocument, "\n\n");
//                                    for (String typeAnnotation : annotations) {
//                                        RUtils.appendToDocument(myPackageDocument, typeAnnotation);
//                                    }
//                                }
//
//                                tempFile.delete(this);
//
//
//                                RType type = RTypeProvider.guessReturnValueTypeFromBody((RFunctionExpression) assignedValue);
//                                if (!RUnknownType.class.isInstance(type)) {
//                                    RUtils.appendToDocument(myPackageDocument, "## @return " + type.toString() + "\n");
//                                } else {
//                                    insertTypeFromHelp(assignee, help);
//                                }
//                            }
//                        } catch (IOException e) {
//                            LOG.error(e);
//                        }
//                    }
//                }

                Set<String> unusedParameters = RStaticAnalyzerHelper.optionalParameters((RFunctionExpression) assignedValue);

                if (!unusedParameters.isEmpty()) {
                    RUtils.appendToDocument(myPackageDocument, "## @optional " + StringUtil.join(unusedParameters, ", ") + "\n");
                }
                RUtils.appendToDocument(myPackageDocument, o.getText() + "\n\n");
                RUtils.saveDocument(myPackageDocument);
                LOG.info("end processing " + myFile.getPath());
            }
        }


        private void insertTypeFromHelp(PsiElement assignee, final RHelp help) throws IOException {
            RType valueType = RSkeletonGeneratorHelper.guessReturnValueTypeFromHelp(help);
            if (!RUnknownType.class.isInstance(valueType)) {
                String valueTempFileName = myFile.getNameWithoutExtension() + "-value-temp.r";
                VirtualFile valueTempFile = myFile.getParent().findOrCreateChildData(this, valueTempFileName);
                final Document valueTempDocument = FileDocumentManager.getInstance().getDocument(valueTempFile);
                if (valueTempDocument != null && help.myExamples != null) {
                    RUtils.appendToDocument(valueTempDocument, help.myExamples);
                    RUtils.saveDocument(valueTempDocument);
                    PsiDocumentManager.getInstance(myProject).commitDocument(valueTempDocument);
                    ValueVisitor valueVisitor = new ValueVisitor(valueType, valueTempFile, assignee.getText());
                    PsiFile valuePsiFile = PsiManager.getInstance(myProject).findFile(valueTempFile);
                    if (valuePsiFile != null && valuePsiFile.isValid()) {
                        valuePsiFile.acceptChildren(valueVisitor);
                        if (valueVisitor.isOk()) {
                            RUtils.appendToDocument(myPackageDocument, "## @return " + valueType.toString() + "\n");
                        }
                    }
                }
                valueTempFile.delete(this);
            }
        }


        class ValueVisitor extends RRecursiveElementVisitor {
            private Boolean myOk = null;
            private final RType myCandidate;
            private final VirtualFile myExamplesFile;
            private final String myFunctionName;


            ValueVisitor(RType candidate, VirtualFile examplesFile, String functionName) {
                myCandidate = candidate;
                myExamplesFile = examplesFile;
                myFunctionName = functionName;
            }


            @Override
            public void visitCallExpression(@NotNull RCallExpression o) {
                if (!myFunctionName.equals(o.getExpression().getName())) {
                    return;
                }
                if (myOk != null && !myOk) {
                    return;
                }
                String packageQuoted = "\"" + myPackageName + "\"";
                String programString = "library(package=" + packageQuoted + ", character.only=TRUE);"
                        + "loadNamespace(package=" + packageQuoted + ");"
                        + "source(\"" + myExamplesFile + "\");"
                        + "myValueType<-class(" + o.getText() + ");"
                        + "print(myValueType)";
                String rPath = RInterpreterService.getInstance().getInterpreterPath();
                try {
                    GeneralCommandLine gcl = new GeneralCommandLine();
                    gcl.withWorkDirectory(myExamplesFile.getParent().getPath());
                    gcl.setExePath(rPath);
                    gcl.addParameter("--slave");
                    gcl.addParameter("-e");
                    gcl.addParameter(programString);

                    final CapturingProcessHandler processHandler = new CapturingProcessHandler(gcl);
                    final ProcessOutput output = processHandler.runProcess(20000);

                    String stdout = output.getStdout();

                    RType evaluatedType = RSkeletonGeneratorHelper.findType(stdout);
                    myOk = RTypeChecker.matchTypes(myCandidate, evaluatedType);
                } catch (ExecutionException | IllegalArgumentException e) {
                    e.printStackTrace();
                }
            }


            public boolean isOk() {
                return myOk != null && myOk;
            }
        }
    }


    static class Visitor extends RVisitor {
        private boolean hasErrors = false;


        @Override
        public void visitCallExpression(@NotNull RCallExpression callExpression) {
            PsiReference referenceToFunction = callExpression.getExpression().getReference();
            if (referenceToFunction != null) {
                PsiElement assignmentStatement = referenceToFunction.resolve();
                if (assignmentStatement != null && assignmentStatement instanceof RAssignmentStatement) {
                    RAssignmentStatement assignment = (RAssignmentStatement) assignmentStatement;
                    RPsiElement assignedValue = assignment.getAssignedValue();
                    if (assignedValue != null && assignedValue instanceof RFunctionExpression) {
                        RFunctionExpression function = (RFunctionExpression) assignedValue;
                        List<RExpression> arguments = callExpression.getArgumentList().getExpressionList();
                        try {
                            RTypeChecker.checkTypes(arguments, function);
                        } catch (MatchingException e) {
                            hasErrors = true;
                        }
                    }
                }
            }
        }


        public boolean hasErrors() {
            return hasErrors;
        }
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
