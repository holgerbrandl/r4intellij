/*
 * Copyright 2011-2011 Gregory Shrago
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

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
import java.util.Set;

import static com.r4intellij.packages.LocalRUtil.basePackages;


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
                    List<RPackage> funPackage = RPackageService.getInstance().getContainingPackages(functionName);
                    List<String> funPackageNames = Lists.newArrayList(Iterables.transform(funPackage, Functions.toStringFunction()));

                    if (funPackageNames.isEmpty())
                        return;

                    // check if there's an import statement for any of them
                    List<String> importedPackages = ((RFile) psiElement.getContainingFile()).getImportedPackages();

                    // todo also include dependencies here
                    Set<RPackage> resolvedImports = RPackageService.getInstance().resolveDependencies(importedPackages);
                    importedPackages = Lists.newArrayList(Iterables.transform(resolvedImports, Functions.toStringFunction()));

                    importedPackages = Lists.newArrayList(Iterables.concat(basePackages, importedPackages));

                    // check whether the import list contains any of the packages

                    if (!Sets.intersection(Sets.newHashSet(importedPackages), Sets.newHashSet(funPackageNames)).isEmpty()) {
                        return;
                    }


                    // no overlap --> highlight as error and suggest to import one!

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
