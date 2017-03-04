package com.r4intellij.typing;

import com.r4intellij.typing.types.*;

/**
 * @author Holger Brandl
 */
public class TypeUtil {

    public static boolean matchTypes(RType type, RType replacementType) {
        return matchTypes(type, replacementType, false);
    }


    public static boolean matchTypes(RType type, RType replacementType, boolean isOptional) {
        if (replacementType instanceof RUnknownType) {
            return true;
        }
        if (type instanceof RUnknownType) {
            return true;
        }
        if (isOptional && replacementType instanceof RNullType) {
            return true;
        }
        if (type instanceof RUnionType) {
            for (RType t : ((RUnionType) type).getTypes()) {
                if (matchTypes(t, replacementType)) {
                    return true;
                }
            }
            return false;
        }
        if (replacementType instanceof RUnionType) {
            for (RType t : ((RUnionType) replacementType).getTypes()) {
                if (!matchTypes(type, t)) {
                    return false;
                }
            }
            return true;
        }
        if (replacementType instanceof RS4ClassType) {
            RType superClass = ((RS4ClassType) replacementType).getSuperClass();
            if (superClass != null && matchTypes(type, superClass)) {
                return true;
            }
        }
        if (type instanceof RListType) {
            if (!RListType.class.isInstance(replacementType)) {
                return false;
            }
            RListType listType = (RListType) type;
            RListType replacementList = (RListType) replacementType;
            for (String field : listType.getFields()) {
                if (!replacementList.hasField(field)) {
                    return false;
                }
                if (!matchTypes(listType.getFieldType(field), replacementList.getFieldType(field))) {
                    return false;
                }
            }
            return true;
        }
        if (replacementType instanceof RNumericType && type instanceof RIntegerType) {
            return true; // yeah yeah
        }
        return type.equals(replacementType) || RTypeProvider.isSubtype(replacementType, type);
    }
}
