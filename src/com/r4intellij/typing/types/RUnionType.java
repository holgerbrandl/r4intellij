package com.r4intellij.typing.types;

import com.r4intellij.psi.api.RExpression;
import com.r4intellij.typing.RTypeProvider;

import java.util.*;

public class RUnionType extends RType {
    private Set<RType> myTypes;


    public Set<RType> getTypes() {
        return myTypes;
    }


    @Override
    public String getCanonicalName() {
        return "union";
    }


    private RUnionType(Set<RType> types) {
        myTypes = types;
    }


    public static RType create(Set<RType> types) {
        if (types.isEmpty()) {
            return RUnknownType.INSTANCE;
        }
        unpackUnions(types);
        types = mergeSimilar(types);
        if (types.size() == 1) {
            return types.iterator().next();
        }

        return new RUnionType(types);
    }


    private static Set<RType> mergeSimilar(Set<RType> types) {
        List<RType> typeList = new ArrayList<RType>(types);
        Map<RType, RType> parentTypes = new HashMap<RType, RType>(types.size());

        //initially each type is itself parent
        for (RType type : types) {
            parentTypes.put(type, type);
        }

        //merge like simple DSU
        for (int i = 0; i < typeList.size(); i++) {
            RType curType = typeList.get(i);
            for (int j = i + 1; j < typeList.size(); j++) {
                RType type = typeList.get(j);
                mergeTypes(parentTypes, curType, type);
            }
        }
        return new HashSet<RType>(parentTypes.values());
    }


    private static void mergeTypes(Map<RType, RType> parentTypes, RType curType, RType type) {
        RType curTypeParent = parentTypes.get(curType);
        RType typeParent = parentTypes.get(type);
        if (RTypeProvider.isSubtype(curTypeParent, typeParent)) {
            parentTypes.put(curType, typeParent);
            return;
        }
        if (RTypeProvider.isSubtype(typeParent, curTypeParent)) {
            parentTypes.put(type, curTypeParent);
        }
    }


    private static void unpackUnions(Set<RType> types) {
        Set<RType> savedTypes = new HashSet<RType>(types);
        types.clear();
        for (RType type : savedTypes) {
            if (type instanceof RUnionType) {
                types.addAll(((RUnionType) type).myTypes);
            } else {
                types.add(type);
            }
        }
    }


    @Override
    public boolean equals(Object o) {
        if (!(o instanceof RUnionType)) {
            return super.equals(o);
        }
        RUnionType type = (RUnionType) o;
        if (type.myTypes.size() != myTypes.size()) {
            return false;
        }
        for (RType t : myTypes) {
            if (!type.myTypes.contains(t)) {
                return false;
            }
        }
        return true;
    }


    public boolean contains(RType type) {
        return myTypes.contains(type);
    }


    @Override
    public String toString() {
        StringBuilder builder = new StringBuilder();
        for (RType type : myTypes) {
            builder.append(type.toString()).append("|");
        }
        builder.deleteCharAt(builder.length() - 1);
        return builder.toString();
    }


    @Override
    public RType getSubscriptionType(List<RExpression> expressions, boolean isSingleBracket) {
        HashSet<RType> subscriptTypes = new HashSet<RType>();
        for (RType type : myTypes) {
            subscriptTypes.add(type.getSubscriptionType(expressions, isSingleBracket));
        }
        return RUnionType.create(subscriptTypes);
    }


    @Override
    public RType afterSubscriptionType(List<RExpression> arguments, RType valueType, boolean isSingle) {
        HashSet<RType> afterTypes = new HashSet<RType>();
        for (RType type : myTypes) {
            afterTypes.add(type.afterSubscriptionType(arguments, valueType, isSingle));
        }
        return RUnionType.create(afterTypes);
    }


    @Override
    public RType getElementTypes() {
        HashSet<RType> elementTypes = new HashSet<RType>();
        for (RType type : myTypes) {
            elementTypes.add(type.getElementTypes());
        }
        return RUnionType.create(elementTypes);
    }


    @Override
    public RUnionType clone() {
        RUnionType result = (RUnionType) super.clone();
        result.myTypes = new HashSet<RType>(myTypes);
        return result;
    }
}
