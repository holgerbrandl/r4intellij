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

import com.intellij.codeInspection.*;
import com.intellij.openapi.components.ServiceManager;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiRecursiveElementWalkingVisitor;
import com.r4intellij.misc.rinstallcache.PackageCache;
import com.r4intellij.misc.rinstallcache.PackageCacheService;
import com.r4intellij.misc.rinstallcache.RCacheUtils;
import com.r4intellij.misc.rinstallcache.RPackage;
import com.r4intellij.psi.RFile;
import com.r4intellij.psi.RFuncall;
import com.r4intellij.psi.RVariable;
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

    @NotNull
    @Override
    public String getShortName() {
        return "MissingPackageImportInspection";
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

                if (psiElement instanceof RFuncall) {
                    RVariable funVar = ((RFuncall) psiElement).getVariable();

                    // is is a locally defined function?
                    if (funVar.getReference() != funVar) {
                        PackageCacheService cacheService = ServiceManager.getService(PackageCacheService.class);
                        PackageCache cache = cacheService.getCache();
                        if (cache != null) {
                            List<String> funPackageNames = getContainingPackages(cache, funVar.getText());

                            // check if there's an import statement for any of them
                            List<String> importedPackages = RCacheUtils.getImportedPackageNames((RFile) psiElement.getContainingFile());

                            // check whether the import list contains any of the packages
                            boolean isImported = false;

                            if (RCacheUtils.containsBasePckg(funPackageNames)) {
                                isImported = true;
                            } else {
                                for (String importedPackage : importedPackages) {
                                    if (funPackageNames.contains(importedPackage)) {
                                        isImported = true;
                                        break;
                                    }
                                }
                            }

                            if (isImported)
                                return;

                            // no overlap --> highlight as error and suggest to import one!

                            List<LocalQuickFix> fixes = new ArrayList<LocalQuickFix>();
                            for (String funPackageName : funPackageNames) {
                                fixes.add(new ImportLibraryFix(funPackageName));
                            }

                            problemsHolder.registerProblem(funVar, "'" + funVar + "' has been detected to belong to a not yet imported package", fixes.toArray(new LocalQuickFix[0]));
                        }
                    }
                }

                super.visitElement(psiElement);
            }
        });
    }

    private static List<String> getContainingPackages(PackageCache cache, String funName) {
        List<RPackage> funPackages = cache.getPackagesOfFunction(funName);
        List<String> funPackageNames = new ArrayList<String>();

        for (RPackage funPackage : funPackages) {
            funPackageNames.add(funPackage.getName());
        }

        return funPackageNames;
    }
}
