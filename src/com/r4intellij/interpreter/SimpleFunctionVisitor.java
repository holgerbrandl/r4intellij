package com.r4intellij.interpreter;

import com.intellij.openapi.editor.Document;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiElement;
import com.r4intellij.RStaticAnalyzerHelper;
import com.r4intellij.psi.api.RAssignmentStatement;
import com.r4intellij.psi.api.RFunctionExpression;
import com.r4intellij.psi.api.RPsiElement;
import com.r4intellij.psi.api.RVisitor;
import org.jetbrains.annotations.NotNull;

import java.util.Set;

/**
 * @author Holger Brandl
 */
public class SimpleFunctionVisitor extends RVisitor {
    private final Document myPackageDocument;
    private final VirtualFile myFile;

    private final String myPackageName;
    private Project myProject;


    public SimpleFunctionVisitor(Document packageDocument, VirtualFile file, Project project, String packageName) {
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

            Set<String> unusedParameters = RStaticAnalyzerHelper.optionalParameters((RFunctionExpression) assignedValue);

            if (!unusedParameters.isEmpty()) {
                TypedFunctionVisitor.appendToDocument(myPackageDocument, "## @optional " + StringUtil.join(unusedParameters, ", ") + "\n");
            }
            TypedFunctionVisitor.appendToDocument(myPackageDocument, o.getText() + "\n\n");
            TypedFunctionVisitor.saveDocument(myPackageDocument);
            RSkeletonGenerator.LOG.info("end processing " + myFile.getPath());
        }
    }
}
