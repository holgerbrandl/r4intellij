package com.r4intellij.inspections;

import com.intellij.codeInspection.InspectionManager;
import com.intellij.codeInspection.ProblemDescriptor;
import com.intellij.codeInspection.ProblemsHolder;
import com.intellij.psi.PsiFile;
import com.r4intellij.RPsiUtils;
import com.r4intellij.intentions.InstallLibraryFix;
import com.r4intellij.intentions.RefreshPackageIndexQuickFix;
import com.r4intellij.packages.RIndexCache;
import com.r4intellij.packages.RPackage;
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
    public String getDisplayName() {
        return "Missing package";
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
            public void visitReferenceExpression(@NotNull RReferenceExpression o) {

                if (RPsiUtils.isNamespacePrefix(o)) {
                    checkPackage(o, problemsHolder);
                }
            }


            @Override
            public void visitCallExpression(@NotNull RCallExpression psiElement) {
                String methodName = psiElement.getExpression().getText();


                if (PACKAGE_IMPORT_METHODS.contains(methodName) &&
                        !psiElement.getArgumentList().getExpressionList().isEmpty()) {

                    RExpression packageExpression = psiElement.getArgumentList().getExpressionList().get(0);
                    checkPackage(packageExpression, problemsHolder);
                }

                String namespacePrefix = getNamespacePrefix(psiElement);
                if (namespacePrefix != null) {
                    checkPackage((RExpression) psiElement.getExpression().getChildren()[0], problemsHolder);
                }


                // needed for child-visits
//                psiElement.acceptChildren(this); // do we always need this?
            }
        });
    }


    private static String getNamespacePrefix(RCallExpression psiElement) {
        RExpression expression = psiElement.getExpression();
        if ((expression instanceof RReferenceExpression))
            return ((RReferenceExpression) expression).getNamespace();

        return null;
    }


    private static void checkPackage(RExpression packageExpression, ProblemsHolder problemsHolder) {
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
        RPackage byName = RIndexCache.getInstance().getByName(packageName);

        if (byName == null) {
            String descriptionTemplate = "'" + packageName + "' is not yet installed";
            problemsHolder.registerProblem(packageExpression, descriptionTemplate, new InstallLibraryFix(packageName));
            problemsHolder.registerProblem(packageExpression, descriptionTemplate, new RefreshPackageIndexQuickFix());
        }
    }


    // http://stackoverflow.com/questions/41298164/how-to-remove-single-and-double-quotes-at-both-ends-of-a-string
    public static String unqote(String text) {
        return text.replaceAll("^['\"]*", "").replaceAll("['\"]*$", "");
    }
}
