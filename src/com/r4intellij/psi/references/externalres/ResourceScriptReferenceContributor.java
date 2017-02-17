package com.r4intellij.psi.references.externalres;

import com.intellij.patterns.PsiElementPattern;
import com.intellij.patterns.StandardPatterns;
import com.intellij.psi.PsiReferenceContributor;
import com.intellij.psi.PsiReferenceRegistrar;
import com.r4intellij.psi.api.RCallExpression;
import com.r4intellij.psi.api.RReferenceExpression;
import com.r4intellij.psi.api.RStringLiteralExpression;
import org.jetbrains.annotations.NotNull;

import static com.intellij.patterns.PlatformPatterns.psiElement;

public class ResourceScriptReferenceContributor extends PsiReferenceContributor {

    @Override
    public void registerReferenceProviders(@NotNull PsiReferenceRegistrar registrar) {
        registrar.registerReferenceProvider(SOURCE_URL_PATTERN, new ResourceScriptReferenceProvider());
    }


    public static final PsiElementPattern.Capture<RStringLiteralExpression> SOURCE_URL_PATTERN =
            psiElement(RStringLiteralExpression.class).
                    withSuperParent(2, psiElement(RCallExpression.class).withChild(psiElement(RReferenceExpression.class).withText(StandardPatterns.string().oneOf("source_url", "devtools::source_url", "source"))));
}