package com.r4intellij.editor;

import com.intellij.openapi.paths.PathReferenceManager;
import com.intellij.psi.*;
import com.intellij.util.ProcessingContext;
import com.r4intellij.psi.RStringLiteralExpressionImpl;
import com.r4intellij.psi.api.RFile;
import org.jetbrains.annotations.NotNull;

import static com.intellij.patterns.PlatformPatterns.psiElement;
import static com.intellij.patterns.PlatformPatterns.psiFile;

public class ResourceFileReferenceProvider extends PsiReferenceContributor {
    @Override
    public void registerReferenceProviders(@NotNull PsiReferenceRegistrar registrar) {
        // for additional patterns see https://intellij-support.jetbrains.com/hc/en-us/community/posts/206114249-How-to-complete-string-literal-expressions-
        registrar.registerReferenceProvider(psiElement(RStringLiteralExpressionImpl.class).inFile(psiFile(RFile.class)),
                new LinkDestinationReferenceProvider());
    }


    private static class LinkDestinationReferenceProvider extends PsiReferenceProvider {
        @NotNull
        @Override
        public PsiReference[] getReferencesByElement(@NotNull PsiElement element, @NotNull ProcessingContext context) {
            return PathReferenceManager.getInstance().createReferences(element, false, false, true);
        }
    }
}
