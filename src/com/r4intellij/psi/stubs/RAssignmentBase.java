package com.r4intellij.psi.stubs;

import com.intellij.psi.stubs.IStubElementType;
import com.r4intellij.psi.RBaseElementImpl;
import com.r4intellij.psi.api.RExpression;
import org.jetbrains.annotations.NotNull;

public abstract class RAssignmentBase extends RBaseElementImpl<RAssignmentStub> implements RExpression {

    public RAssignmentBase(@NotNull com.intellij.lang.ASTNode node) {
        super(node);
    }


    public RAssignmentBase(@NotNull RAssignmentStub stub, @NotNull IStubElementType nodeType) {
        super(stub, nodeType);
    }
}
