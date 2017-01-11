package com.r4intellij.typing.types;


import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.util.Function;
import com.intellij.util.containers.Predicate;
import com.r4intellij.RPsiUtils;
import com.r4intellij.psi.api.*;
import com.r4intellij.typing.RTypeProvider;
import org.jetbrains.annotations.Nullable;

import java.util.*;

public class RS4ClassType extends RType {

  private String myName;
  private Map<String, RType> mySlots;
  private RType mySuperClass;

  public RS4ClassType(String name, Map<String, RType> slots, RType superClass) {
    myName = name;
    mySlots = slots;
    mySuperClass = superClass;
  }

  @Override
  public String getCanonicalName() {
    return "S4(" + myName + ", " + StringUtil.join(mySlots.entrySet(), new Function<Map.Entry<String, RType>, String>() {
      @Override
      public String fun(Map.Entry<String, RType> entry) {
        return entry.getKey() + ": " + entry.getValue().getName();
      }
    }, ",") + (mySuperClass != null ? " : " + mySuperClass.getName() : "") + ")";
  }

  public RType getSuperClass() {
    return mySuperClass;
  }

  public boolean hasSlot(String slot) {
    return mySlots.containsKey(slot);
  }

  public RType getSlotType(String slot) {
    if (mySlots.containsKey(slot)) {
      return mySlots.get(slot);
    }
    if (mySuperClass != null && mySuperClass instanceof RS4ClassType) {
      return mySuperClass.getSlotType(slot);
    }
    return new RErrorType(toString() + " don't have slot " + slot);
  }

  public Collection<String> getSlots() {
    return mySlots.keySet();
  }

  @Nullable
  public static RS4ClassType createFromSetClass(RCallExpression callExpression) {
    return createFromSetClass(callExpression, new HashSet<String>());
  }

  @Nullable
  private static RS4ClassType createFromSetClass(RCallExpression callExpression, Set<String> recursionGuard) {
    Map<String, RExpression> params = RPsiUtils.findParameterValues(callExpression, "Class", "slots", "representation", "contains");

    String name = null;
    Map<String, RType> representation = new HashMap<String, RType>();
    RType superClass = null;

    RExpression nameExpression = params.get("Class");
    if (nameExpression != null && nameExpression instanceof RStringLiteralExpression) {
      name = nameExpression.getText().substring(1, nameExpression.getText().length() - 1);
    }

    if (name == null) {
      return null;
    }
    recursionGuard.add(name);

    RExpression representationExpression = params.get("slots");
    if (representationExpression == null) {
      representationExpression = params.get("representation");
    }
    if (representationExpression != null && representationExpression instanceof RCallExpression) {
      RCallExpression representationCall = ((RCallExpression)representationExpression);
      String functionName = representationCall.getExpression().getName();
      if ("c".equals(functionName) || "representation".equals(functionName)) {
        for (RExpression slot : representationCall.getArgumentList().getExpressionList()) {
          if (slot instanceof RStringLiteralExpression) {
            String slotName = slot.getText().substring(1, slot.getText().length() - 1);
            representation.put(slotName, RUnknownType.INSTANCE);
            continue;
          }
          if (!RAssignmentStatement.class.isInstance(slot)) {
            continue;
          }
          RAssignmentStatement slotAssignment = ((RAssignmentStatement)slot);
          RPsiElement assignedValue = slotAssignment.getAssignedValue();
          if (!RStringLiteralExpression.class.isInstance(assignedValue)) {
            continue;
          }
          String slotName = slotAssignment.getAssignee().getText();
          String slotTypeName = assignedValue.getText().substring(1, assignedValue.getText().length() - 1);
          RType slotType = RTypeProvider.findTypeByName(slotTypeName);
          if (slotType == null) {
            slotType = RS4ClassType.byName(callExpression.getProject(), slotTypeName);
          }
          if (slotType == null) {
            slotType = RUnknownType.INSTANCE;
          }
          representation.put(slotName, slotType);
        }
      }
    }

    RExpression superClassExpression = params.get("contains");
    if (superClassExpression != null && superClassExpression instanceof RStringLiteralExpression) {
      String superClassName = superClassExpression.getText().substring(1, superClassExpression.getText().length() - 1);
      superClass = RTypeProvider.findTypeByName(superClassName);
      if (superClass == null) {
        superClass = RS4ClassType.byName(callExpression.getProject(), superClassName);
      }
      if (superClass == null) {
        superClass = RUnknownType.INSTANCE;
      }
    }
    return new RS4ClassType(name, representation, superClass);
  }

  public static RS4ClassType byName(Project project, final String name) {
    return byName(project, name, new HashSet<String>());
  }

  private static RS4ClassType byName(Project project, final String name, Set<String> recursionGuard) {
    if (recursionGuard.contains(name)) {
      return null;
    }
    RCallExpression call = RPsiUtils.findCall(project, "setClass", new Predicate<RCallExpression>() {
      @Override
      public boolean apply(@Nullable RCallExpression input) {
        RExpression s4Class = RPsiUtils.findParameterValue("Class", input);
        if (s4Class == null || !RStringLiteralExpression.class.isInstance(s4Class)) {
          return false;
        }
        String text = s4Class.getText();
        return name.equals(text.substring(1, text.length() - 1));
      }
    });
    return call != null ? RS4ClassType.createFromSetClass(call, recursionGuard) : null;
  }

  @Override
  public RS4ClassType clone() {
    RS4ClassType s4 = (RS4ClassType)super.clone();
    s4.mySlots = new HashMap<String, RType>(mySlots);
    return s4;
  }

  @Override
  public RType getSubscriptionType(List<RExpression> expressions, boolean isSingleBracket) {
    if (mySuperClass == null) {
      return RUnknownType.INSTANCE;
    }
    return mySuperClass.getSubscriptionType(expressions, isSingleBracket);
  }

  @Override
  public RType afterSubscriptionType(List<RExpression> arguments, RType valueType, boolean isSingle) {
    if (mySuperClass == null) {
      return RUnknownType.INSTANCE;
    }
    return mySuperClass.afterSubscriptionType(arguments, valueType, isSingle);
  }

  @Override
  public RType getElementTypes() {
    if (mySuperClass != null) {
      return super.getElementTypes();
    }
    return new RErrorType("Wrong sequence type");
  }

  @Override
  public RType getMemberType(String tag) {
    if (mySuperClass != null) {
      return mySuperClass.getMemberType(tag);
    }
    return new RErrorType("$ isn't defined for S4 Classes");
  }

  @Override
  public RType afterMemberType(String tag, RType valueType) {
    if (mySuperClass == null) {
      return new RErrorType("$ isn't defined for S4 class");
    }
    if (mySuperClass instanceof RListType) { // R is really works this way :(
      RS4ClassType cloned = clone();
      cloned.mySuperClass = mySuperClass.afterMemberType(tag, valueType);
      return cloned;
    }
    return mySuperClass.afterMemberType(tag, valueType);
  }
}
