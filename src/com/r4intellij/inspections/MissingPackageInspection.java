package com.r4intellij.inspections;

import com.intellij.codeHighlighting.HighlightDisplayLevel;
import com.intellij.codeInspection.InspectionManager;
import com.intellij.codeInspection.ProblemDescriptor;
import com.intellij.codeInspection.ProblemsHolder;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiRecursiveElementWalkingVisitor;
import com.r4intellij.intentions.InstallLibraryFix;
import com.r4intellij.packages.RPackage;
import com.r4intellij.packages.RPackageService;
import com.r4intellij.psi.api.RCallExpression;
import com.r4intellij.psi.api.RExpression;
import com.r4intellij.psi.api.RFile;
import org.jetbrains.annotations.Nls;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import javax.swing.*;

import static com.r4intellij.editor.RCompletionContributor.PACKAGE_IMPORT_METHODS;


/**
 * @author Holger Brandl
 */
public class MissingPackageInspection extends RInspection {

    @Nullable
    @Override
    public JComponent createOptionsPanel() {
        return super.createOptionsPanel();
    }


    @Nls
    @NotNull
    @Override
    public String getGroupDisplayName() {
        return "R";

    }


    @Nls
    @NotNull
    @Override
    public String getDisplayName() {
        return "Missing package";
    }



    @NotNull
    @Override
    public HighlightDisplayLevel getDefaultLevel() {
        return HighlightDisplayLevel.ERROR;
    }


    @Override
    public ProblemDescriptor[] checkFile(@NotNull PsiFile file, @NotNull InspectionManager manager, boolean isOnTheFly) {
        ProblemsHolder problemsHolder = new ProblemsHolder(manager, file, isOnTheFly);
        checkFile(file, problemsHolder);
        return problemsHolder.getResultsArray();
    }


    private static void checkFile(final PsiFile file, final ProblemsHolder problemsHolder) {
        if (!(file instanceof RFile)) return;

        file.accept(new PsiRecursiveElementWalkingVisitor() {
            @Override
            public void visitElement(PsiElement psiElement) {

                if (psiElement instanceof RCallExpression) {
                    String methodName = ((RCallExpression) psiElement).getExpression().getText();

                    if (PACKAGE_IMPORT_METHODS.contains(methodName) &&
                            !((RCallExpression) psiElement).getArgumentList().getExpressionList().isEmpty()) {

                        RExpression packageExpression = ((RCallExpression) psiElement).getArgumentList().getExpressionList().get(0);

                        String packageName = packageExpression.getText();
                        RPackage byName = RPackageService.getInstance().getByName(packageName);

                        if (byName == null) {
                            String descriptionTemplate = "'" + packageName + "' is not yet installed";
                            problemsHolder.registerProblem(packageExpression, descriptionTemplate, new InstallLibraryFix(packageName));
                        }
                    }
                }

                super.visitElement(psiElement);
            }
//
        });
    }
}
