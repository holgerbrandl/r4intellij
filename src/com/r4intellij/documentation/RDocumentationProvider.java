package com.r4intellij.documentation;

import com.google.common.base.Strings;
import com.google.common.collect.Iterables;
import com.google.common.collect.Lists;
import com.intellij.lang.documentation.AbstractDocumentationProvider;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.r4intellij.packages.RHelperUtil;
import com.r4intellij.packages.RPackage;
import com.r4intellij.packages.RPackageService;
import com.r4intellij.psi.RReferenceExpressionImpl;
import com.r4intellij.psi.api.*;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.List;
import java.util.stream.Collectors;

import static com.r4intellij.interpreter.RSkeletonGenerator.SKELETON_DIR_NAME;
import static com.r4intellij.packages.RHelperUtil.LOG;
import static com.r4intellij.packages.RHelperUtil.runHelperWithArgs;

/**
 * For local function definitions provide doc string documentation (using docstring)
 * For library functions use R help.
 */
public class RDocumentationProvider extends AbstractDocumentationProvider {
    @Nullable
    @Override
    public String getQuickNavigateInfo(PsiElement element, PsiElement originalElement) {
        return super.getQuickNavigateInfo(element, originalElement);
    }


    @Nullable
    @Override
    public String generateDoc(PsiElement reference, @Nullable PsiElement identifier) {
        // check if it's a library function and return help if it is

        if (identifier == null) return null;

        String elementText = identifier.getText();

        // first guess : process locally defined function definitions
        if (!isLibraryElement(reference)) {
            if (reference instanceof RAssignmentStatement) {
                RPsiElement assignedValue = ((RAssignmentStatement) reference).getAssignedValue();

                if (assignedValue instanceof RFunctionExpression) {
                    String docString = ((RFunctionExpression) assignedValue).getDocStringValue();
                    return docString != null ? docString : "No doc-string found for locally defined function.";
                }
            }
        }

        // ease R documentation lookup by detecting package if possible
        String packageName = null;

        // if possible resolve package from namespace prefix
        if (reference instanceof RReferenceExpressionImpl &&
                ((RReferenceExpressionImpl) reference).getNamespace() != null) {
            packageName = ((RReferenceExpressionImpl) reference).getNamespace();

        }

        // resolve method package from script dependencies
        if (packageName == null) {
            List<RPackage> origins = resolvePckgFromContext(elementText, identifier.getContainingFile());
            if (!origins.isEmpty()) {
                packageName = Iterables.getLast(origins).getName();
            }
        }

        // also make sure that we can provide help in skeleton files
        if (packageName == null && isLibraryElement(reference)) {
            packageName = reference.getContainingFile().getVirtualFile().getName().replaceAll(".r$", "");
        }

        // run generic R help on symbol
        return getHelpForFunction(elementText, packageName);
    }


    private boolean isFunctionName(@Nullable PsiElement element1) {
        return element1 != null &&
                element1.getParent() != null &&
                element1.getParent().getParent() instanceof RCallExpression;
    }


    public static boolean isLibraryElement(PsiElement element) {
        return element != null &&
                Strings.nullToEmpty(
                        element.getContainingFile().getVirtualFile().getCanonicalPath()
                ).contains(SKELETON_DIR_NAME);
    }


    /**
     * If packageName parameter equals null we do not load package
     */
    @Nullable
    public static String getHelpForFunction(@NotNull final String assignee, @Nullable final String packageName) {
        if (assignee.isEmpty()) {
            return null;
        }

        final RHelperUtil.PluginResourceFile helpHelper = new RHelperUtil.PluginResourceFile(packageName != null ? "r-help.r" : "r-help-without-package.r");

        LOG.info("fetching help with" + helpHelper.getFile());

        String[] args = packageName != null ? new String[]{packageName, assignee} : new String[]{assignee};
        RHelperUtil.RRunResult runResult = runHelperWithArgs(helpHelper, args);
        if (runResult == null) return null;

        String stdout = runResult.getStdOut();

        if (stdout.startsWith("No documentation")) {
            return null;
        }


        if (StringUtil.isNotEmpty(stdout)) {
            return new RHelpParser(stdout).getFormattedString();
        }

        return null;
    }


    private List<RPackage> resolvePckgFromContext(String functionName, PsiFile psiFile) {
        // todo add Rmd chunk support here
        // todo this should also take the position into account since not all import may be done in the script header
        if (!(psiFile instanceof RFile)) {
            return Lists.newArrayList();
        }
        List<String> importNames = ((RFile) psiFile).getImportedPackages();
        List<RPackage> imports = RPackageService.getInstance().resolveDependencies(importNames);

        // filter for those that contain given function
        return imports.stream().distinct().filter(p -> p.hasFunction(functionName)).collect(Collectors.toList());
    }
}
