package com.r4intellij.psi.stubs;

import com.intellij.psi.stubs.IStubElementType;
import com.intellij.psi.stubs.StubBase;
import com.intellij.psi.stubs.StubElement;
import com.r4intellij.psi.api.RAssignmentStatement;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

public class RAssignmentStubImpl extends StubBase<RAssignmentStatement> implements RAssignmentStub {
    private final String myName;
    private final boolean isFunction;


    public RAssignmentStubImpl(@Nullable final String name,
                               @NotNull final StubElement parent,
                               @NotNull IStubElementType stubElementType,
                               boolean isFunctionDefinition) {
        super(parent, stubElementType);
        myName = name;
        isFunction = isFunctionDefinition;
    }


    @Override
    public String getName() {
        return myName;
    }


    @Override
    public String toString() {
        return "RAssignmentStub(" + myName + ")";
    }


    @Override
    public boolean isFunctionDeclaration() {
        return isFunction;
    }
}
