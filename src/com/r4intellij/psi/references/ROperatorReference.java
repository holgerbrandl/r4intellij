package com.r4intellij.psi.references;

import com.intellij.openapi.util.TextRange;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiPolyVariantReference;
import com.intellij.psi.ResolveResult;
import com.intellij.util.IncorrectOperationException;
import com.r4intellij.psi.api.ROperator;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.ArrayList;
import java.util.List;

public class ROperatorReference implements PsiPolyVariantReference {

    private final ROperator myElement;


    public ROperatorReference(ROperator element) {
        myElement = element;
    }


    @NotNull
    @Override
    public ResolveResult[] multiResolve(boolean incompleteCode) {
        List<ResolveResult> result = new ArrayList<ResolveResult>();

        String text = myElement.getText();
//        String doubleQuotedText = "\"" + text + "\"";
//        String singleQuotedText = "'" + text + "'";
//        String backquotedText = "`" + text + "`";

        RResolver.resolveInFileOrLibrary(myElement, text, result);

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
        return new ResolveResultWrapper(myElement, false, multiResolve(false)).getBest();
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
        return null;
    }


    @Override
    public PsiElement bindToElement(@NotNull PsiElement element) throws IncorrectOperationException {
        return null;
    }


    @Override
    public boolean isReferenceTo(PsiElement element) {
        return resolve() == element;
    }


    @NotNull
    @Override
    public Object[] getVariants() {
        return new Object[0];
    }


    @Override
    public boolean isSoft() {
        return false;
    }
}
