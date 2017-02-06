package com.r4intellij.inspections;

import com.google.common.base.Functions;
import com.google.common.base.Joiner;
import com.google.common.collect.Iterables;
import com.google.common.collect.Lists;
import com.google.common.collect.Sets;
import com.intellij.codeInspection.LocalInspectionToolSession;
import com.intellij.codeInspection.LocalQuickFix;
import com.intellij.codeInspection.ProblemHighlightType;
import com.intellij.codeInspection.ProblemsHolder;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiElementVisitor;
import com.intellij.psi.util.PsiTreeUtil;
import com.r4intellij.RPsiUtils;
import com.r4intellij.editor.RCompletionContributor;
import com.r4intellij.intentions.ImportLibraryFix;
import com.r4intellij.packages.RPackage;
import com.r4intellij.packages.RPackageService;
import com.r4intellij.parsing.RElementTypes;
import com.r4intellij.psi.api.*;
import com.r4intellij.psi.references.RReferenceImpl;
import org.jetbrains.annotations.Nls;
import org.jetbrains.annotations.NotNull;

import java.util.ArrayList;
import java.util.List;

public class UnresolvedReferenceInspection extends RInspection {
    @Nls
    @NotNull
    @Override
    public String getDisplayName() {
        return "Unresolved reference";
    }


    @NotNull
    @Override
    public PsiElementVisitor buildVisitor(@NotNull ProblemsHolder holder, boolean isOnTheFly, @NotNull LocalInspectionToolSession session) {
        return new ReferenceVisitor(holder);
    }


    private class ReferenceVisitor extends RVisitor {

        private final ProblemsHolder myProblemHolder;


        public ReferenceVisitor(@NotNull ProblemsHolder holder) {
            myProblemHolder = holder;
        }


        @Override
        public void visitReferenceExpression(@NotNull RReferenceExpression element) {
            PsiElement sibling = element.getNextSibling();
            if (sibling != null && sibling.getNode().getElementType() == RElementTypes.R_DOUBLECOLON) {
                return;
            }

            if (RPsiUtils.isNamedArgument(element)) {
                return;
            }

            // for calls like myfun=function(a, ...) a; myfun(23, b=4) don't care about b **[todo]** unit-test?
            RCallExpression callExpression = PsiTreeUtil.getParentOfType(element, RCallExpression.class);
            if (callExpression != null) {
                RFunctionExpression function = RPsiUtils.getFunction(callExpression);
                if (function != null) {
                    List<RParameter> list = function.getParameterList().getParameterList();
                    if (RPsiUtils.containsTripleDot(list)) {
                        return;
                    }
                }
            }

            // ignore function calls here because they are handled by the missing import inspection
            if (element.getParent() instanceof RCallExpression) {
                if (resolveInPackages((RCallExpression) element.getParent(), myProblemHolder)) {
                    // we could find it in a package or locally so it's somehow resolvable but not yet imported
                    return;
                }
            }


            // prevent package names to show up as unresolved
            // todo should we resolve packages to skeletons
            if (callExpression != null && RCompletionContributor.PACKAGE_IMPORT_METHODS.contains(callExpression.getExpression().getName())) {
                return;
            }

            RReferenceImpl reference = element.getReference();
            if (reference != null) {
                PsiElement resolve = reference.resolve();
                if (resolve == null) {
                    myProblemHolder.registerProblem(element, "Unresolved reference", ProblemHighlightType.GENERIC_ERROR_OR_WARNING);
                }
            }
        }


        // todo do once all current unit-tests are fixed:
        // todo thise should go into the RResolver and complement or replace com.r4intellij.psi.references.RResolver.resolveFunction*() !!
        private boolean resolveInPackages(RCallExpression psiElement, ProblemsHolder problemsHolder) {
            // is it a locally defined function?
            RFunctionExpression function = RPsiUtils.getFunction(psiElement);

            boolean isLocalFunction = function != null && function.getContainingFile().equals(psiElement.getContainingFile());
            if (isLocalFunction) {
                return true;
            }

            RPackageService packageService = RPackageService.getInstance();
//            if (!packageService.isReady()) return true; // fixme!

            // search packages by function name
            RExpression funExpr = psiElement.getExpression();
            String functionName = funExpr.getText();

            List<RPackage> funPackage = packageService.getContainingPackages(functionName);
            List<String> funPackageNames = Lists.newArrayList(Iterables.transform(funPackage, Functions.toStringFunction()));

            if (funPackageNames.isEmpty())
                return false;

            // check if there's an import statement for any of them
            List<String> importedPackages = ((RFile) psiElement.getContainingFile()).getImportedPackages();

            List<RPackage> resolvedImports = packageService.resolveDependencies(importedPackages);
            importedPackages = Lists.newArrayList(Iterables.transform(resolvedImports, Functions.toStringFunction()));

            // check whether the import list contains any of the packages
            if (!Sets.intersection(Sets.newHashSet(importedPackages), Sets.newHashSet(funPackageNames)).isEmpty()) {
                return true;
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
                    Joiner.on(", ").join(funPackageNames) + ") which does not seem to be imported yet.";
            problemsHolder.registerProblem(funExpr, descriptionTemplate, fixes.toArray(new LocalQuickFix[0]));

            return true;
        }

    }
}
