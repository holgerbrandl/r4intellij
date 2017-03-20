package com.r4intellij.psi.stubs;

import com.intellij.lang.ASTNode;
import com.intellij.psi.PsiElement;
import com.intellij.psi.stubs.*;
import com.intellij.util.io.StringRef;
import com.r4intellij.psi.RAssignmentStatementImpl;
import com.r4intellij.psi.api.RAssignmentStatement;
import com.r4intellij.psi.api.RFile;
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


    @NotNull
    @Override
    public RAssignmentStub createStub(@NotNull RAssignmentStatement psi, StubElement parentStub) {
        final String name = psi.getName();
        final RPsiElement value = psi.getAssignedValue();

//        assert value != null;

//        if(value == null  || value.getParent()==null || value.getParent().getParent() == null){
//            System.err.println("invalid stub element:" + psi);
//        }

        boolean isTopLevelAssign = value.getParent() != null && value.getParent().getParent() != null && value.getParent().getParent() instanceof RFile;

        return new RAssignmentStubImpl(name, parentStub, this, value instanceof RFunctionExpression, isTopLevelAssign);
    }


    @Override
    public void serialize(@NotNull final RAssignmentStub stub, @NotNull final StubOutputStream dataStream)
            throws IOException {
        dataStream.writeName(stub.getName());
        dataStream.writeBoolean(stub.isFunctionDeclaration());
        dataStream.writeBoolean(stub.isTopLevelAssignment());
    }


    @Override
    @NotNull
    public RAssignmentStub deserialize(@NotNull final StubInputStream dataStream, final StubElement parentStub) throws IOException {
        String name = StringRef.toString(dataStream.readName());
        final boolean isFunctionDefinition = dataStream.readBoolean();
        final boolean isTopLevel = dataStream.readBoolean();
        return new RAssignmentStubImpl(name, parentStub, this, isFunctionDefinition, isTopLevel);
    }


    @Override
    public void indexStub(@NotNull final RAssignmentStub stub, @NotNull final IndexSink sink) {
        final String name = stub.getName();
        if (name != null && stub.getParentStub() instanceof PsiFileStub && stub.isTopLevelAssignment()) {
            sink.occurrence(RAssignmentNameIndex.KEY, name);
        }
    }
}
