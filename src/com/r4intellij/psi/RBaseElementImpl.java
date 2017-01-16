package com.r4intellij.psi;

import com.intellij.extapi.psi.StubBasedPsiElementBase;
import com.intellij.lang.ASTNode;
import com.intellij.lang.Language;
import com.intellij.psi.PsiElementVisitor;
import com.intellij.psi.stubs.IStubElementType;
import com.intellij.psi.stubs.StubElement;
import com.r4intellij.RFileType;
import com.r4intellij.psi.api.RPsiElement;
import org.jetbrains.annotations.NotNull;

public class RBaseElementImpl<T extends StubElement> extends StubBasedPsiElementBase<T> implements RPsiElement {
    public RBaseElementImpl(@NotNull final ASTNode node) {
        super(node);
    }


    public RBaseElementImpl(@NotNull final T stub, @NotNull final IStubElementType nodeType) {
        super(stub, nodeType);
    }


    @NotNull
    @Override
    public Language getLanguage() {
        return RFileType.INSTANCE.getLanguage();
    }


    @Override
    public String toString() {
        return getNode().getElementType().toString();
    }


    @Override
    public void accept(@NotNull PsiElementVisitor visitor) {
        visitor.visitElement(this);
    }
}
