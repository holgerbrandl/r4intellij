package com.r4intellij.typing.types;

import com.intellij.openapi.util.text.StringUtil;
import com.intellij.util.Function;
import com.r4intellij.psi.api.RExpression;
import com.r4intellij.psi.api.RNumericLiteralExpression;
import com.r4intellij.psi.api.RStringLiteralExpression;

import java.util.*;

public class RListType extends RType {

  private static class TypeDescriptor {
    static final TypeDescriptor UNKNOWN = new TypeDescriptor(null, RNullType.INSTANCE);
    final String myName;
    final RType myType;

    private TypeDescriptor(String name, RType type) {
      myName = name;
      myType = type;
    }
  }

  private Map<String, RType> myFields = new HashMap<String, RType>();
  private List<TypeDescriptor> myPositionalTypes = new ArrayList<TypeDescriptor>();
  //shows if we know all fields
  private final boolean myPrecise;

  public RListType() {
    this(true);
  }

  public RListType(boolean isPrecise) {
    myPrecise = isPrecise;
  }

  public RListType(RListType other) {
    myFields.putAll(other.myFields);
    myPositionalTypes.addAll(other.myPositionalTypes);
    myPrecise = other.myPrecise;
  }

  @Override
  public RListType clone() {
    RListType result = (RListType)super.clone();
    result.myFields = new HashMap<String, RType>(myFields);
    result.myPositionalTypes = new ArrayList<TypeDescriptor>(myPositionalTypes);
    return result;
  }

  @Override
  public String getCanonicalName() {
    if (myPrecise) {
      return "list(" + StringUtil.join(myPositionalTypes, new Function<TypeDescriptor, String>() {
        @Override
        public String fun(TypeDescriptor descriptor) {
          String typeName = descriptor.myType.getName();
          String name = descriptor.myName;
          return (name == null) ? typeName : name + ": " + typeName;
        }
      }, ", ") + ")";
    }
    else {
      return "list{" + StringUtil.join(myFields.entrySet(), new Function<Map.Entry<String, RType>, String>() {
        @Override
        public String fun(Map.Entry<String, RType> entry) {
          return entry.getKey() + ": " + entry.getValue().getName();
        }
      }, ", ") + "}";
    }
  }

  public void addField(String name, int index, RType fieldType) {
    if (name != null) {
      myFields.put(name, fieldType);
    }
    if (myPrecise) {
      while (myPositionalTypes.size() <= index) {
        myPositionalTypes.add(TypeDescriptor.UNKNOWN);
      }
      if (index >= 0) {
        myPositionalTypes.set(index, new TypeDescriptor(name, fieldType));
      }
    }
  }

  public void addField(int index, RType fieldType) {
    String name = null;
    if (index >= 0 && index < myPositionalTypes.size()) {
      name = myPositionalTypes.get(index).myName;
    }
    addField(name, index, fieldType);
  }

  public void addField(String name, RType fieldType) {
    int index = myPositionalTypes.size();
    if (name != null && myPrecise) {
      for (int i = 0; i < myPositionalTypes.size(); i++) {
        if (name.equals(myPositionalTypes.get(i).myName)) {
          index = i;
          break;
        }
      }
    }
    addField(name, index, fieldType);
  }

  public void addField(RType fieldType) {
    addField(null, fieldType);
  }

  public void removeField(String name) {
    myFields.remove(name);
    if (name != null && myPrecise) {
      Iterator<TypeDescriptor> it = myPositionalTypes.iterator();
      while (it.hasNext()) {
        TypeDescriptor desc = it.next();
        if (name.equals(desc.myName)) {
          it.remove();
          return;
        }
      }
    }
  }

  public void removeField(int position) {
    if (myPrecise && position >= 0 && position < myPositionalTypes.size()) {
      String name = myPositionalTypes.get(position).myName;
      myPositionalTypes.remove(position);
      myFields.remove(name);
    }
  }

  public RType getFieldType(String name) {
    if (myFields.containsKey(name)) {
      return myFields.get(name);
    }
    String partialMatching = null;
    for (String field : myFields.keySet()) {
      if (field.startsWith(name)) {
        if (partialMatching != null) {
          return RUnknownType.INSTANCE;
        }
        partialMatching = field;
      }
    }
    if (partialMatching != null) {
      return myFields.get(partialMatching);
    }
    return myPrecise ? RNullType.INSTANCE : RUnknownType.INSTANCE;
  }


  private TypeDescriptor getFieldDescriptor(int position) {
    if (myPrecise && position >= 0 && position < myPositionalTypes.size()) {
      return myPositionalTypes.get(position);
    }
    return TypeDescriptor.UNKNOWN;
  }

  @Override
  public RType getSubscriptionType(List<RExpression> indices, boolean isSingleBracket) {
    if (indices.isEmpty()) {
      return this;
    }
    if (indices.size() > 1) {
      return RUnknownType.INSTANCE;
    }
    RExpression index = indices.get(0);
    RType type = RUnknownType.INSTANCE;
    String indexName = null;
    if (index instanceof RStringLiteralExpression) {
      String quoted = index.getText();
      indexName = quoted.substring(1, quoted.length() - 1);
      type = getFieldType(indexName);
    }
    if (index instanceof RNumericLiteralExpression) {
      try {
        int i = Integer.parseInt(index.getText());
        TypeDescriptor descriptor = getFieldDescriptor(i - 1);
        indexName = descriptor.myName;
        type = descriptor.myType;
      }
      catch (NumberFormatException e) {
        // Do nothing
      }
    }
    if (!RNullType.class.isInstance(type) && !RUnknownType.class.isInstance(type) && isSingleBracket) {
      RListType listType = new RListType();
      listType.addField(indexName, type);
      type = listType;
    }
    return type;
  }

  // handles assignment to subscription expression
  @Override
  public RType afterSubscriptionType(List<RExpression> indices, RType valueType, boolean isSingleBracket) {
    if (indices.isEmpty()) {
      return this;
    }
    if (indices.size() > 1) {
      return RUnknownType.INSTANCE;
    }
    if (isSingleBracket && valueType instanceof RListType) {
      RListType list = (RListType)valueType;
      if (!list.myPrecise) {
        return RUnknownType.INSTANCE;
      }
      if (list.myPositionalTypes.isEmpty()) {
        return RUnknownType.INSTANCE;
      }
      valueType = list.myPositionalTypes.get(0).myType;
    }
    RExpression index = indices.get(0);
    RListType resultType = new RListType(this);
    if (index instanceof RStringLiteralExpression) {
      String quoted = index.getText();
      String indexName = quoted.substring(1, quoted.length() - 1);
      if (valueType instanceof RNullType) {
        resultType.removeField(indexName);
      }
      else {
        resultType.addField(indexName, valueType);
      }
    }
    if (index instanceof RNumericLiteralExpression) {
      try {
        int i = Integer.parseInt(index.getText());
        if (valueType instanceof RNullType) {
          resultType.removeField(i - 1);
        }
        else {
          resultType.addField(i - 1, valueType);
        }
      }
      catch (NumberFormatException e) {
        // do nothing
      }
    }
    return resultType;
  }

  @Override
  public RType getElementTypes() {
    if (!myPrecise) {
      return RUnknownType.INSTANCE;
    }
    Set<RType> elementTypes = new HashSet<RType>();
    for (TypeDescriptor descriptor : myPositionalTypes) {
      elementTypes.add(descriptor.myType);
    }
    return RUnionType.create(elementTypes);
  }

  @Override
  public RType getMemberType(String tag) {
    return getFieldType(tag);
  }

  @Override
  public RType afterMemberType(String tag, RType valueType) {
    RListType clone = clone();
    if (valueType instanceof RNullType) {
      clone.removeField(tag);
    }
    else {
      clone.addField(tag, valueType);
    }
    return clone;
  }

  public Collection<String> getFields() {
    return myFields.keySet();
  }

  public boolean hasField(String field) {
    return !myPrecise || myFields.containsKey(field);
  }
}
