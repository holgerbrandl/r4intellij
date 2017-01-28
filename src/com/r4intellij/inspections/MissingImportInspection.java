
package com.r4intellij.inspections;

import com.google.common.base.Functions;
import com.google.common.base.Joiner;
import com.google.common.collect.Iterables;
import com.google.common.collect.Lists;
import com.google.common.collect.Sets;
import com.intellij.codeInspection.*;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiRecursiveElementWalkingVisitor;
import com.r4intellij.packages.RPackage;
import com.r4intellij.packages.RPackageService;
import com.r4intellij.psi.api.RCallExpression;
import com.r4intellij.psi.api.RExpression;
import com.r4intellij.psi.api.RFile;
import org.jetbrains.annotations.Nls;
import org.jetbrains.annotations.NotNull;

import java.util.ArrayList;
import java.util.List;


/**
 * @author Holger Brandl
 */
public class MissingImportInspection extends LocalInspectionTool {

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
        return "Missing package import";
    }

    @Override
    public boolean isEnabledByDefault() {
        return true;
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
                    RExpression funExpr = ((RCallExpression) psiElement).getExpression();
                    String functionName = funExpr.getText();

                    // is is a locally defined function?
//                    if (functionName.getReference() != functionName) {
                    RPackageService packageService = RPackageService.getInstance();
                    if (!packageService.isReady()) return;

                    List<RPackage> funPackage = packageService.getContainingPackages(functionName);
                    List<String> funPackageNames = Lists.newArrayList(Iterables.transform(funPackage, Functions.toStringFunction()));

                    if (funPackageNames.isEmpty())
                        return;

                    // check if there's an import statement for any of them
                    List<String> importedPackages = ((RFile) psiElement.getContainingFile()).getImportedPackages();

                    List<RPackage> resolvedImports = packageService.resolveDependencies(importedPackages);
                    importedPackages = Lists.newArrayList(Iterables.transform(resolvedImports, Functions.toStringFunction()));

                    // check whether the import list contains any of the packages
                    if (!Sets.intersection(Sets.newHashSet(importedPackages), Sets.newHashSet(funPackageNames)).isEmpty()) {
                        return;
                    }


                    // no overlap --> highlight as error and suggest to import one!


                    // Also provide importing packages as options (like tidyverse for mutate)
                    // DISABLED: works but is adding too many confusing options
//                    Set<String> funPckgByImport = funPackageNames.stream().
//                            flatMap(pName -> packageService.getImporting(packageService.getByName(pName)).stream()).
//                            map(RPackage::getName).
//                            collect(Collectors.toSet());
//                    funPackageNames.addAll(funPckgByImport);


                    List<LocalQuickFix> fixes = new ArrayList<LocalQuickFix>();
                    for (String funPackageName : funPackageNames) {
                        fixes.add(new ImportLibraryFix(funPackageName));
                    }

                    String descriptionTemplate = "'" + functionName + "' has been detected in a package (" +
                            Joiner.on(", ").join(funPackageNames) + ") which doesn't seem to be imported yet.";
                    problemsHolder.registerProblem(funExpr, descriptionTemplate, fixes.toArray(new LocalQuickFix[0]));
                }

                super.visitElement(psiElement);
            }
//
        });
    }
}
