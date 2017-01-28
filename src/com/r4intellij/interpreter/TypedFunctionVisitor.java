package com.r4intellij.interpreter;

import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.diagnostic.Logger;
import com.intellij.openapi.editor.Document;
import com.intellij.openapi.fileEditor.FileDocumentManager;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.*;
import com.intellij.util.DocumentUtil;
import com.intellij.util.FileContentUtil;
import com.r4intellij.RHelp;
import com.r4intellij.RStaticAnalyzerHelper;
import com.r4intellij.documentation.RDocumentationProvider;
import com.r4intellij.packages.RHelperUtil;
import com.r4intellij.psi.RRecursiveElementVisitor;
import com.r4intellij.psi.api.*;
import com.r4intellij.typing.*;
import com.r4intellij.typing.types.RType;
import com.r4intellij.typing.types.RUnknownType;
import org.jetbrains.annotations.NotNull;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * @author Holger Brandl
 */
class TypedFunctionVisitor extends RVisitor {

    protected static final Logger LOG = Logger.getInstance("#" + TypedFunctionVisitor.class.getName());

    private final Document myPackageDocument;
    private final VirtualFile myFile;
    private final String myPackageName;
    private Project myProject;


    public TypedFunctionVisitor(Document packageDocument, VirtualFile file, Project project, String packageName) {
        myPackageDocument = packageDocument;
        myFile = file;
        myProject = project;
        myPackageName = packageName;
    }


    public static void appendToDocument(@NotNull final Document document, final String text) {
        DocumentUtil.writeInRunUndoTransparentAction(new Runnable() {
            @Override
            public void run() {
                document.insertString(document.getTextLength(), text);
            }
        });
    }


    public static void saveDocument(@NotNull final Document document) {
        ApplicationManager.getApplication().runWriteAction(new Runnable() {
            @Override
            public void run() {
                FileDocumentManager.getInstance().saveDocument(document);
            }
        });
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

            String helpText = RDocumentationProvider.getHelpForFunction(assignee.getText(), myPackageName);
            // todo uncomment once skeletoniziation is more stable and speedy
            if (helpText != null) {
                RHelp help = new RHelp(helpText);

                final Map<RParameter, RType> parsedTypes = RSkeletonGeneratorHelper.guessArgsTypeFromHelp(help,
                        (RFunctionExpression) assignedValue);

                if (parsedTypes != null && !parsedTypes.isEmpty()) {
                    String tempFileName = myFile.getNameWithoutExtension() + "temp.r";
                    try {
                        VirtualFile tempFile = myFile.getParent().findOrCreateChildData(this, tempFileName);
                        final Document tempDocument = FileDocumentManager.getInstance().getDocument(tempFile);
                        if (tempDocument != null) {
                            final List<String> annotations = new ArrayList<String>();
                            for (Map.Entry<RParameter, RType> entry : parsedTypes.entrySet()) {
                                String typeAnnotation = DocStringUtil.generateTypeAnnotation(entry.getKey(), entry.getValue());
                                appendToDocument(tempDocument, typeAnnotation);
                                annotations.add(typeAnnotation);
                            }
                            appendToDocument(tempDocument, o.getText() + "\n");

                            if (help.myExamples != null && !help.myExamples.isEmpty()) {
                                appendToDocument(tempDocument, "\n" + help.myExamples + "\n");
                            }

                            if (help.myUsage != null && !help.myUsage.isEmpty()) {
                                appendToDocument(tempDocument, "\n" + help.myUsage + "\n");
                            }
                            saveDocument(tempDocument);
                            PsiDocumentManager.getInstance(myProject).commitDocument(tempDocument);
                            Visitor visitor = new Visitor();
                            FileContentUtil.reparseFiles(tempFile);
                            PsiFile psiFile = PsiManager.getInstance(myProject).findFile(tempFile);
                            if (psiFile != null && psiFile.isValid()) {
                                psiFile.acceptChildren(visitor);
                            }
                            if (!visitor.hasErrors()) {
                                appendToDocument(myPackageDocument, "\n\n");
                                for (String typeAnnotation : annotations) {
                                    appendToDocument(myPackageDocument, typeAnnotation);
                                }
                            }

                            tempFile.delete(this);


                            RType type = RTypeProvider.guessReturnValueTypeFromBody((RFunctionExpression) assignedValue);
                            if (!RUnknownType.class.isInstance(type)) {
                                appendToDocument(myPackageDocument, "## @return " + type.toString() + "\n");
                            } else {
                                insertTypeFromHelp(assignee, help);
                            }
                        }
                    } catch (IOException e) {
                        LOG.error(e);
                    }
                }
            }

            Set<String> unusedParameters = RStaticAnalyzerHelper.optionalParameters((RFunctionExpression) assignedValue);

            if (!unusedParameters.isEmpty()) {
                appendToDocument(myPackageDocument, "## @optional " + StringUtil.join(unusedParameters, ", ") + "\n");
            }
            appendToDocument(myPackageDocument, o.getText() + "\n\n");
            saveDocument(myPackageDocument);
            RSkeletonGenerator.LOG.info("end processing " + myFile.getPath());
        }
    }


    private void insertTypeFromHelp(PsiElement assignee, final RHelp help) throws IOException {
        RType valueType = RSkeletonGeneratorHelper.guessReturnValueTypeFromHelp(help);
        if (!RUnknownType.class.isInstance(valueType)) {
            String valueTempFileName = myFile.getNameWithoutExtension() + "-value-temp.r";
            VirtualFile valueTempFile = myFile.getParent().findOrCreateChildData(this, valueTempFileName);
            final Document valueTempDocument = FileDocumentManager.getInstance().getDocument(valueTempFile);
            if (valueTempDocument != null && help.myExamples != null) {
                appendToDocument(valueTempDocument, help.myExamples);
                saveDocument(valueTempDocument);
                PsiDocumentManager.getInstance(myProject).commitDocument(valueTempDocument);
                ValueVisitor valueVisitor = new ValueVisitor(valueType, valueTempFile, assignee.getText());
                PsiFile valuePsiFile = PsiManager.getInstance(myProject).findFile(valueTempFile);
                if (valuePsiFile != null && valuePsiFile.isValid()) {
                    valuePsiFile.acceptChildren(valueVisitor);
                    if (valueVisitor.isOk()) {
                        appendToDocument(myPackageDocument, "## @return " + valueType.toString() + "\n");
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

            String stdout = RHelperUtil.runCommand(programString);
            String rPath = RInterpreterService.getInstance().getInterpreterPath();

            RType evaluatedType = RSkeletonGeneratorHelper.findType(stdout);
            myOk = RTypeChecker.matchTypes(myCandidate, evaluatedType);

        }


        public boolean isOk() {
            return myOk != null && myOk;
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
}
