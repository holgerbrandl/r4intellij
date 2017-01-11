package com.r4intellij.psi.stubs;

import com.intellij.lang.ASTNode;
import com.intellij.psi.PsiElement;
import com.intellij.psi.stubs.IStubElementType;
import com.intellij.psi.stubs.IndexSink;
import com.intellij.psi.stubs.StubElement;
import com.r4intellij.RFileType;
import com.r4intellij.psi.api.RPsiElement;
import org.jetbrains.annotations.NonNls;
import org.jetbrains.annotations.NotNull;

public abstract class RStubElementType<StubT extends StubElement, PsiT extends RPsiElement> extends IStubElementType<StubT, PsiT> {
  public RStubElementType(@NonNls final String debugName) {
    super(debugName, RFileType.INSTANCE.getLanguage());
  }

  public abstract PsiElement createElement(@NotNull final ASTNode node);

  @Override
  public void indexStub(@NotNull final StubT stub, @NotNull final IndexSink sink) {
  }

  @Override
  @NotNull
  public String getExternalId() {
    return "ther." + super.toString();
  }
}
