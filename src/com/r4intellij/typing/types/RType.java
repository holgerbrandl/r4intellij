package com.r4intellij.typing.types;

import com.intellij.openapi.util.text.StringUtil;
import com.r4intellij.psi.api.RExpression;
import com.r4intellij.typing.RTypeEnvironment;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

public abstract class RType implements Cloneable {
    private List<String> myS3Classes = new ArrayList<String>();


    public static RType getMaxType(List<RType> types, RTypeEnvironment env) {
        Set<RType> maxTypes = new HashSet<RType>();
        for (RType type : types) {
            RType resolvedType = type.resolveType(env);
            Set<RType> currentTypes = new HashSet<RType>();
            if (resolvedType instanceof RUnionType) {
                currentTypes.addAll(((RUnionType) resolvedType).getTypes());
            } else if (resolvedType instanceof RListType) {
                currentTypes.add(new RListType(false));
            } else {
                currentTypes.add(resolvedType);
            }
            if (maxTypes.isEmpty()) {
                maxTypes.addAll(currentTypes);
            } else {
                Set<RType> newMaxTypes = new HashSet<RType>();
                for (RType maxType : maxTypes) {
                    for (RType currentType : currentTypes) {
                        boolean currentIsMore = RType.getOrder(currentType) > RType.getOrder(maxType);
                        newMaxTypes.add(currentIsMore ? currentType : maxType);
                    }
                }
                maxTypes = newMaxTypes;
            }
        }
        return RUnionType.create(maxTypes);
    }


    public static int getOrder(RType type) {
        if (type instanceof RNullType) {
            return -1;
        } else if (type instanceof RRawType) {
            return 0;
        } else if (type instanceof RLogicalType) {
            return 1;
        } else if (type instanceof RIntegerType) {
            return 2;
        } else if (type instanceof RNumericType) {
            return 3;
        } else if (type instanceof RComplexType) {
            return 4;
        } else if (type instanceof RCharacterType) {
            return 5;
        } else if (type instanceof RUnknownType) {
            return 6;
        } else if (type instanceof RListType) {
            return 7;
        } else {
            throw new IllegalArgumentException("Incorrect type: " + type.getName());
        }
    }


    @Override
    public RType clone() {
        try {
            RType result = (RType) super.clone();
            result.myS3Classes = new ArrayList<String>(myS3Classes);
            return result;
        } catch (CloneNotSupportedException e) {
            throw new RuntimeException(e);
        }
    }


    public final String getName() {
        if (myS3Classes.isEmpty()) {
            return getCanonicalName();
        }
        return getCanonicalName() + "[" + StringUtil.join(myS3Classes, ", ") + "]";
    }


    public abstract String getCanonicalName();


    @SuppressWarnings("SimplifiableIfStatement")
    @Override
    public boolean equals(Object o) {
        if (o == null || !(o instanceof RType)) {
            return false;
        }
        return ((RType) o).getName().equals(getName());
    }


    public RType resolveType(RTypeEnvironment env) {
        return clone();
    }


    public RType getSubscriptionType(List<RExpression> expressions, boolean isSingleBracket) {
        return RUnknownType.INSTANCE;
    }


    @Override
    public int hashCode() {
        return getName().hashCode();
    }


    @Override
    public String toString() {
        return getName();
    }


    public RType afterSubscriptionType(List<RExpression> arguments, RType valueType, boolean isSingleBracket) {
        // TODO : valueType is union
        if (arguments.isEmpty()) {
            return this;
        }
        return RUnknownType.INSTANCE;
    }


    public RType getElementTypes() {
        return RUnknownType.INSTANCE;
    }


    public List<String> getS3Classes() {
        return new ArrayList<String>(myS3Classes);
    }


    public RType replaceS3Types(List<String> s3Classes) {
        RType result = clone();
        result.myS3Classes = new ArrayList<String>(s3Classes);
        return result;
    }


    public RType getMemberType(String tag) {
        return RUnknownType.INSTANCE;
    }


    public RType getSlotType(String tag) {
        return new RErrorType(toString() + " don't have slots");
    }


    public RType afterMemberType(String tag, RType valueType) {
        RListType result = new RListType(false);
        result.addField(tag, valueType);
        return result;
    }
}
