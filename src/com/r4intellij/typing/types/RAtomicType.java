package com.r4intellij.typing.types;

import com.r4intellij.psi.api.RExpression;

import java.util.List;

public abstract class RAtomicType extends RType {
    @Override
    public RType getSubscriptionType(List<RExpression> expressions, boolean isSingleBracket) {
        return this;
    }


    @Override
    public RType afterSubscriptionType(List<RExpression> arguments, RType valueType, boolean isSingle) {
        if (arguments.isEmpty()) {
            return this;
        }
        if (valueType instanceof RAtomicType) {
            return RType.getOrder(this) > RType.getOrder(valueType) ? this : valueType;
        }
        return super.afterSubscriptionType(arguments, valueType, isSingle);
    }


    @Override
    public RType getElementTypes() {
        return this;
    }


    @Override
    public RType getMemberType(String tag) {
        return new RErrorType("$ operator is invalid for atomic vectors");
    }
}
