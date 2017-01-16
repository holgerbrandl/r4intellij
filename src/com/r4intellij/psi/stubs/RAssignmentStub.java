package com.r4intellij.psi.stubs;

import com.intellij.psi.stubs.NamedStub;
import com.r4intellij.psi.api.RAssignmentStatement;

public interface RAssignmentStub extends NamedStub<RAssignmentStatement> {
    public boolean isFunctionDeclaration();
}
