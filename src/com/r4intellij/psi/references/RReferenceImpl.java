package com.r4intellij.psi.references;

import com.intellij.codeInsight.lookup.LookupElement;
import com.intellij.codeInsight.lookup.LookupElementBuilder;
import com.intellij.lang.ASTNode;
import com.intellij.lang.Language;
import com.intellij.openapi.util.TextRange;
import com.intellij.psi.*;
import com.intellij.psi.impl.light.LightElement;
import com.intellij.util.IncorrectOperationException;
import com.r4intellij.RElementGenerator;
import com.r4intellij.RLanguage;
import com.r4intellij.RPsiUtils;
import com.r4intellij.packages.RIndexCache;
import com.r4intellij.packages.RPackage;
import com.r4intellij.parsing.RElementTypes;
import com.r4intellij.psi.RElementFactory;
import com.r4intellij.psi.api.RFile;
import com.r4intellij.psi.api.RReferenceExpression;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;

import static com.r4intellij.editor.RCompletionContributor.isPackageContext;
import static com.r4intellij.psi.references.RResolver.*;

public class RReferenceImpl implements PsiPolyVariantReference {
    protected final RReferenceExpression myElement;


    public RReferenceImpl(RReferenceExpression element) {
        myElement = element;
    }


    @NotNull
    @Override
    public ResolveResult[] multiResolve(boolean incompleteCode) {
        final List<ResolveResult> result = new ArrayList<ResolveResult>();

        if (RPsiUtils.isNamedArgument(myElement)) {
            resolveNameArgument(myElement, myElement.getName(), result); // only usage
            return result.toArray(new ResolveResult[result.size()]);
        }

        final String elementName = myElement.getName();
        if (elementName == null) return ResolveResult.EMPTY_ARRAY;

        final String namespace = myElement.getNamespace();
        if (namespace != null) {
            resolveWithNamespace(myElement.getProject(), elementName, namespace, result);
        }

        resolveFunctionCall(myElement, elementName, result);
        if (!result.isEmpty()) {
            return result.toArray(new ResolveResult[result.size()]);
        }

//        if (!result.isEmpty()) {
        RResolver.resolveInFileOrLibrary(myElement, elementName, result);
//        }

        // is still empty also include forward references
        if (result.isEmpty()) {
            FileContextResolver forwardResolver = new FileContextResolver();
            forwardResolver.setForwardRefs(true);
            result.addAll(forwardResolver.resolveFromInner(myElement, myElement, elementName));
        }

        return result.toArray(new ResolveResult[result.size()]);
    }


    @Override
    public PsiElement getElement() {
        return myElement;
    }


    @Override
    public TextRange getRangeInElement() {
        final TextRange range = myElement.getNode().getTextRange();
        return range.shiftRight(-myElement.getNode().getStartOffset());
    }


    @Nullable
    @Override
    public PsiElement resolve() {
        return resolve(false).getBest();
    }


    @NotNull
    public ResolveResultWrapper resolve(boolean includeFwdRefs) {
        return new ResolveResultWrapper(myElement, includeFwdRefs, multiResolve(false));
    }


    @NotNull
    @Override
    public String getCanonicalText() {
        return getElement().getText();
    }


    @Override
    public PsiElement handleElementRename(String newElementName) throws IncorrectOperationException {
        final ASTNode oldNameIdentifier = getElement().getNode().findChildByType(RElementTypes.R_IDENTIFIER);
        if (oldNameIdentifier != null) {
            final PsiFile dummyFile = RElementGenerator.createDummyFile(newElementName, false, getElement().getProject());
            ASTNode identifier = dummyFile.getNode().getFirstChildNode().findChildByType(RElementTypes.R_IDENTIFIER);
            if (identifier != null) {
                getElement().getNode().replaceChild(oldNameIdentifier, identifier);
            }
        }
        return getElement();
    }


    @Override
    public PsiElement bindToElement(@NotNull PsiElement element) throws IncorrectOperationException {
        return null;
    }


    @Override
    public boolean isReferenceTo(PsiElement element) {
        // method is called e.g. by ReferencesSearch.search(com.intellij.psi.PsiElement)
        // when searching for unused parameters and variables like in
        // this is typically invoked on all references with the same name

        //TODO: check some conditions to speed up search (ie avoid resolving)
//        if(element instanceof RParameter){
//            // same file at least
//            PsiTreeUtil.
//
//            // better: test for same function expression
//
//        }

        // most other impl do something like
//        final PsiManager manager = getManager();
//        for (final ResolveResult result : multiResolve(false)) {
//            if (manager.areElementsEquivalent(result.getElement(), element)) return true;
//        }

        // this seems enough here
        return resolve() == element;
    }


    // somehow needed to provide reference completion
    // http://www.jetbrains.org/intellij/sdk/docs/reference_guide/custom_language_support/code_completion.html
    @NotNull
    @Override
    public Object[] getVariants() {
        List<LookupElement> completionResults = new ArrayList<LookupElement>();

        if (isPackageContext(myElement)) {         // .. auto-completion for require and libary

//            List<RepoPackage> allPackages = LocalRUtil.getPckgNameVersionMap();
            Set<RPackage> allPackages = RIndexCache.getInstance().getPackages();

            // TODO add completion for not-yet-installed packages

            for (RPackage p : allPackages) {
                completionResults.add(LookupElementBuilder.create(p.getName()).
                        withTypeText(p.getTitle()));
            }
        } else {

            PsiFile containingFile = myElement.getContainingFile();
            if (containingFile instanceof RFile) {
                List<String> importedPackages = ((RFile) containingFile).getImportedPackages(myElement);

                for (String pckg : importedPackages) {
                    RPackage byName = RIndexCache.getInstance().getByName(pckg);
                    if (byName != null) {
                        byName.getFunctionNames()
                                .stream().filter(it -> !it.contains(".__"))
                                .forEach(funName -> {
                                    if (funName == null) System.err.println("funName is null");
                                    completionResults.add(LookupElementBuilder
                                                    .create(new RefLookupElement(myElement.getManager(), RLanguage.getInstance(), pckg + "::" + funName), funName)
                                                    .withTypeText(pckg)
//                                        .withLookupString("base::attach"))
                                    );
                                });
                    }
                }

            }
//            if(parameters.isExtendedCompletion()){

//            }

//            RFile rFile = PsiTreeUtil.getContextOfType(insertedElement, RFile.class);
        }

        return completionResults.toArray();
    }


    // see https://intellij-support.jetbrains.com/hc/en-us/community/posts/206124949/comments/206152455
    public static class RefLookupElement extends LightElement {

        private final String refExpression;


        protected RefLookupElement(@NotNull PsiManager manager, @NotNull Language language, String refExpression) {
            super(manager, language);
            this.refExpression = refExpression;
        }


        public PsiElement getRefExpression() {
            return RElementFactory.createRefExpression(getProject(), refExpression).getReference().resolve().getParent();
        }


        @Override
        public String toString() {
            return null;
        }
    }


    @Override
    public boolean isSoft() {
        return false;
    }
}
