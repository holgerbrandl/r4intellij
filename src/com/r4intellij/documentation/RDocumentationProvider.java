package com.r4intellij.documentation;

import com.google.common.collect.Iterables;
import com.google.common.collect.Lists;
import com.intellij.lang.documentation.AbstractDocumentationProvider;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.r4intellij.RHelp;
import com.r4intellij.RPsiUtils;
import com.r4intellij.packages.RPackage;
import com.r4intellij.packages.RPackageService;
import com.r4intellij.psi.api.RCallExpression;
import com.r4intellij.psi.api.RFile;
import com.r4intellij.psi.api.RFunctionExpression;
import org.jetbrains.annotations.Nullable;

import java.util.List;
import java.util.stream.Collectors;

import static com.r4intellij.interpreter.RSkeletonGenerator.SKELETON_DIR_NAME;

/**
 * For local function definitions provide doc string documentation (using docstring)
 * For library functions use R help.
 */
public class RDocumentationProvider extends AbstractDocumentationProvider {

    @Nullable
    @Override
    public String generateDoc(PsiElement element, @Nullable PsiElement element1) {
        // check if it's a library function and return help if it is
        boolean isSkeleton = element != null && element.getContainingFile().getVirtualFile().getCanonicalPath().contains(SKELETON_DIR_NAME);

        if (isSkeleton &&
                element1 != null &&
                element1.getParent() != null &&
                element1.getParent().getParent() instanceof RCallExpression) {

            String elementText = element1.getText();

//            RPackageService.getInstance().resolveDependencies()
            String packageName = null;

            List<RPackage> origins = resolvePckgFromContext(elementText, element1.getContainingFile());
            if (!origins.isEmpty()) {
                packageName = Iterables.getLast(origins).getName();
            }


            String helpText = RPsiUtils.getHelpForFunction(elementText, packageName);

            if (helpText == null) {
                return null;
            } else {
                return RDocumentationUtils.getFormattedString(new RHelp(helpText));
            }
        }

        // process locally defined function definitions
        for (PsiElement el : element.getChildren()) {
            if (el instanceof RFunctionExpression) {
                final String docString = ((RFunctionExpression) el).getDocStringValue();
                return docString != null ? docString : "No doc-string for locally defined function";
            }
        }


        return null;
    }


    private List<RPackage> resolvePckgFromContext(String functionName, PsiFile psiFile) {
        // todo add Rmd chunk support here
        if (!(psiFile instanceof RFile)) {
            return Lists.newArrayList();
        }
        List<String> importNames = ((RFile) psiFile).getImportedPackages();
        List<RPackage> imports = RPackageService.getInstance().resolveDependencies(importNames);

        // filter for those that contain given function
        return imports.stream().distinct().filter(p -> p.hasFunction(functionName)).collect(Collectors.toList());
    }
}
