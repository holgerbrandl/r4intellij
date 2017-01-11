package com.r4intellij.psi.stubs;

import com.intellij.lang.ASTNode;
import com.intellij.psi.PsiElement;
import com.intellij.psi.stubs.*;
import com.intellij.util.io.StringRef;
import com.r4intellij.psi.RAssignmentStatementImpl;
import com.r4intellij.psi.api.RAssignmentStatement;
import com.r4intellij.psi.api.RFunctionExpression;
import com.r4intellij.psi.api.RPsiElement;
import org.jetbrains.annotations.NotNull;

import java.io.IOException;

public class RAssignmentElementType extends RStubElementType<RAssignmentStub, RAssignmentStatement> {
  public RAssignmentElementType(@NotNull final String debugName) {
    super(debugName);
  }

  @Override
  public PsiElement createElement(@NotNull final ASTNode node) {
    return new RAssignmentStatementImpl(node);
  }

  @Override
  public RAssignmentStatement createPsi(@NotNull final RAssignmentStub stub) {
    return new RAssignmentStatementImpl(stub, this);
  }

  @Override
  public RAssignmentStub createStub(@NotNull RAssignmentStatement psi, StubElement parentStub) {
    final String name = psi.getName();
    final RPsiElement value = psi.getAssignedValue();
    return new RAssignmentStubImpl(name, parentStub, this, value instanceof RFunctionExpression);
  }

  @Override
  public void serialize(@NotNull final RAssignmentStub stub, @NotNull final StubOutputStream dataStream)
    throws IOException {
    dataStream.writeName(stub.getName());
    dataStream.writeBoolean(stub.isFunctionDeclaration());
  }

  @Override
  @NotNull
  public RAssignmentStub deserialize(@NotNull final StubInputStream dataStream, final StubElement parentStub) throws IOException {
    String name = StringRef.toString(dataStream.readName());
    final boolean isFunctionDefinition = dataStream.readBoolean();
    return new RAssignmentStubImpl(name, parentStub, this, isFunctionDefinition);
  }

  @Override
  public void indexStub(@NotNull final RAssignmentStub stub, @NotNull final IndexSink sink) {
    final String name = stub.getName();
    if (name != null && stub.getParentStub() instanceof PsiFileStub && stub.isFunctionDeclaration()) {
      sink.occurrence(RAssignmentNameIndex.KEY, name);
    }
  }
}
