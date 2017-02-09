package com.r4intellij.documentation;

import com.google.common.base.Strings;
import com.google.common.collect.Iterables;
import com.google.common.collect.Lists;
import com.intellij.lang.documentation.AbstractDocumentationProvider;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.r4intellij.RHelp;
import com.r4intellij.packages.RHelperUtil;
import com.r4intellij.packages.RPackage;
import com.r4intellij.packages.RPackageService;
import com.r4intellij.psi.api.RCallExpression;
import com.r4intellij.psi.api.RFile;
import com.r4intellij.psi.api.RFunctionExpression;
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
    public String generateDoc(PsiElement reference, @Nullable PsiElement identifier) {
        // check if it's a library function and return help if it is

        if (identifier == null) return null;

        String elementText = identifier.getText();

        // first guess : process locally defined function definitions
        for (PsiElement el : reference.getChildren()) {
            if (el instanceof RFunctionExpression) {
                String docString = ((RFunctionExpression) el).getDocStringValue();
                return docString != null ? docString : "No doc-string found for locally defined function.";
//                if(docString != null) {
//                    return docString;
//                }
            }
        }


        // try R help by detecting package if possible
        String packageName = null;

        // first be as local as possible taking imported packages into account
        List<RPackage> origins = resolvePckgFromContext(elementText, identifier.getContainingFile());
        if (!origins.isEmpty()) {
            packageName = Iterables.getLast(origins).getName();
        }

        // otherwise detect help via
        if (packageName == null && isLibraryElement(reference)) {
            packageName = reference.getContainingFile().getVirtualFile().getName().replaceAll(".r$", "");
        }

        // run help detection
        String helpText = getHelpForFunction(elementText, packageName);

        if (helpText != null) {
            return RDocumentationUtils.getFormattedString(new RHelp(helpText));
        }

        return null;
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

        return stdout;
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
