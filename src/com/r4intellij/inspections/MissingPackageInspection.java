package com.r4intellij.inspections;

import com.intellij.codeHighlighting.HighlightDisplayLevel;
import com.intellij.codeInspection.InspectionManager;
import com.intellij.codeInspection.ProblemDescriptor;
import com.intellij.codeInspection.ProblemsHolder;
import com.intellij.psi.PsiFile;
import com.r4intellij.intentions.InstallLibraryFix;
import com.r4intellij.packages.RPackage;
import com.r4intellij.packages.RPackageService;
import com.r4intellij.psi.RRecursiveElementVisitor;
import com.r4intellij.psi.api.*;
import org.jetbrains.annotations.Nls;
import org.jetbrains.annotations.NotNull;

import static com.r4intellij.editor.RCompletionContributor.PACKAGE_IMPORT_METHODS;


/**
 * @author Holger Brandl
 */
public class MissingPackageInspection extends RInspection {


    @Nls
    @NotNull
    @Override
    public String getGroupDisplayName() {
        return "R";
    }


// FIXME   https://intellij-support.jetbrains.com/hc/en-us/community/posts/115000074350-Why-are-default-xml-plugin-settings-preferred-over-code-based-component-configuration-


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

        file.accept(new RRecursiveElementVisitor() {

            @Override
            public void visitCallExpression(@NotNull RCallExpression psiElement) {
                String methodName = psiElement.getExpression().getText();

                if (PACKAGE_IMPORT_METHODS.contains(methodName) &&
                        !psiElement.getArgumentList().getExpressionList().isEmpty()) {

                    RExpression packageExpression = psiElement.getArgumentList().getExpressionList().get(0);


                    // support quoted and unquoted method names here
                    String packageName;
                    if (packageExpression instanceof RStringLiteralExpression) {
                        packageName = unqote(packageExpression.getText());
                    } else if (packageExpression instanceof RReferenceExpression) {
                        packageName = packageExpression.getText();
                    } else {
                        // could be a function RCallExpression or something weired, so ignore it
                        return;
                    }
                    RPackage byName = RPackageService.getInstance().getByName(packageName);

                    if (byName == null) {
                        String descriptionTemplate = "'" + packageName + "' is not yet installed";
                        problemsHolder.registerProblem(packageExpression, descriptionTemplate, new InstallLibraryFix(packageName));
                    }
                }
            }
//
        });
    }


    // http://stackoverflow.com/questions/41298164/how-to-remove-single-and-double-quotes-at-both-ends-of-a-string
    public static String unqote(String text) {
        return text.replaceAll("^['\"]*", "").replaceAll("['\"]*$", "");
    }
}
