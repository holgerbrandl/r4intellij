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

import com.intellij.codeInspection.InspectionManager;
import com.intellij.codeInspection.LocalInspectionTool;
import com.intellij.codeInspection.ProblemDescriptor;
import com.intellij.codeInspection.ProblemsHolder;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiRecursiveElementWalkingVisitor;
import com.r4intellij.packages.RPackage;
import com.r4intellij.packages.RPackageService;
import com.r4intellij.psi.api.RCallExpression;
import com.r4intellij.psi.api.RFile;
import org.jetbrains.annotations.Nls;
import org.jetbrains.annotations.NotNull;

import static com.r4intellij.editor.RCompletionContributor.PACKAGE_IMPORT_METHODS;


/**
 * @author Holger Brandl
 */
public class MissingPackageInspection extends LocalInspectionTool {

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
        return "Missing Package";
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
                    String methodName = ((RCallExpression) psiElement).getExpression().getText();

                    if (PACKAGE_IMPORT_METHODS.contains(methodName) &&
                            !((RCallExpression) psiElement).getArgumentList().getExpressionList().isEmpty()) {

                        String packageName = ((RCallExpression) psiElement).getArgumentList().getExpressionList().get(0).getText();

                        RPackage byName = RPackageService.getInstance().getByName(packageName);

                        if (byName == null) {
                            String descriptionTemplate = "'" + packageName + "' is not yet installed";
                            problemsHolder.registerProblem(psiElement, descriptionTemplate, new InstallLibraryFix(packageName));
                        }
                    }
                }

                super.visitElement(psiElement);
            }
//
        });
    }
}
