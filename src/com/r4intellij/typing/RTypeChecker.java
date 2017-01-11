package com.r4intellij.typing;

import com.r4intellij.psi.api.RAssignmentStatement;
import com.r4intellij.psi.api.RExpression;
import com.r4intellij.psi.api.RFunctionExpression;
import com.r4intellij.psi.api.RParameter;
import com.r4intellij.typing.types.*;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class RTypeChecker {

  public static void checkTypes(List<RExpression> arguments, RFunctionExpression functionExpression) throws MatchingException {
    Map<RExpression, RParameter> matchedParams = new HashMap<RExpression, RParameter>();
    List<RExpression> matchedByTripleDot = new ArrayList<RExpression>();
    RType type = RTypeProvider.getType(functionExpression);
    if (!RFunctionType.class.isInstance(type)) {
      return; // TODO: fix me properly
    }
    RFunctionType functionType = (RFunctionType)type;
    matchArgs(arguments, matchedParams, matchedByTripleDot, functionType);
    for (Map.Entry<RExpression, RParameter> entry : matchedParams.entrySet()) {
      RParameter parameter = entry.getValue();
      RType paramType = RTypeProvider.getParamType(parameter, functionType);
      if (paramType == null || paramType instanceof RUnknownType) {
        continue;
      }
      boolean isOptional = functionType.isOptional(parameter.getName());
      RType argType = RTypeProvider.getType(entry.getKey());
      if (argType != null && !RUnknownType.class.isInstance(argType)) {
        if (!matchTypes(paramType, argType, isOptional)) {
          throw new MatchingException(parameter.getText() + " expected to be of type " + paramType +
                                      ", found type " + argType);
        }
      }
    }
  }

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
      for (RType t : ((RUnionType)type).getTypes()) {
        if (matchTypes(t, replacementType)) {
          return true;
        }
      }
      return false;
    }
    if (replacementType instanceof RUnionType) {
      for (RType t : ((RUnionType)replacementType).getTypes()) {
        if (!matchTypes(type, t)) {
          return false;
        }
      }
      return true;
    }
    if (replacementType instanceof RS4ClassType) {
      RType superClass = ((RS4ClassType)replacementType).getSuperClass();
      if (superClass != null && matchTypes(type, superClass)) {
        return true;
      }
    }
    if (type instanceof RListType) {
      if (!RListType.class.isInstance(replacementType)) {
        return false;
      }
      RListType listType = (RListType)type;
      RListType replacementList = (RListType)replacementType;
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

  public static void matchArgs(List<RExpression> arguments,
                               Map<RExpression, RParameter> matchedParams,
                               List<RExpression> matchedByTripleDot,
                               RFunctionType functionType) throws MatchingException {
    List<RParameter> formalArguments = functionType.getFormalArguments();
    List<RExpression> suppliedArguments = new ArrayList<RExpression>(arguments);
    exactMatching(formalArguments, suppliedArguments, matchedParams);
    partialMatching(formalArguments, suppliedArguments, matchedParams);
    positionalMatching(formalArguments, suppliedArguments, matchedParams, matchedByTripleDot, functionType);
  }

  static void partialMatching(List<RParameter> formalArguments,
                              List<RExpression> suppliedArguments,
                              Map<RExpression, RParameter> matchedParams) throws MatchingException {
    matchParams(formalArguments, suppliedArguments, true, matchedParams);
  }

  static void exactMatching(List<RParameter> formalArguments,
                            List<RExpression> suppliedArguments,
                            Map<RExpression, RParameter> matchedParams) throws MatchingException {
    matchParams(formalArguments, suppliedArguments, false, matchedParams);
  }

  static void positionalMatching(List<RParameter> formalArguments,
                                 List<RExpression> suppliedArguments,
                                 Map<RExpression, RParameter> matchedParams,
                                 List<RExpression> matchedByTripleDot,
                                 RFunctionType functionType) throws MatchingException {
    List<RExpression> matchedArguments = new ArrayList<RExpression>();
    List<RParameter> matchedParameter = new ArrayList<RParameter>();
    int suppliedSize = suppliedArguments.size();
    boolean wasTripleDot = false;
    for (int i = 0; i < formalArguments.size(); i++) {
      RParameter param = formalArguments.get(i);
      if (param.getText().equals("...")) {
        wasTripleDot = true;
        break;
      }
      if (i >= suppliedSize) {
        break;
      }
      RExpression arg = suppliedArguments.get(i);
      if (arg instanceof RAssignmentStatement && ((RAssignmentStatement)arg).isEqual()) {
        String argName = ((RAssignmentStatement)arg).getAssignee().getText();
        if (!argName.equals(param.getName())) {
          wasTripleDot = true;
          break;
        }
      }
      matchedArguments.add(arg);
      matchedParameter.add(param);
      matchedParams.put(arg, param);
    }

    formalArguments.removeAll(matchedParameter);
    suppliedArguments.removeAll(matchedArguments);

    if (wasTripleDot) {
      matchedByTripleDot.addAll(suppliedArguments);
      suppliedArguments.clear();
    }

    List<RParameter> unmatched = new ArrayList<RParameter>();
    for (RParameter parameter : formalArguments) {
      if (parameter.getText().equals("...")) {
        continue;
      }
      RExpression defaultValue = parameter.getExpression();
      if (defaultValue != null) {
        matchedParams.put(defaultValue, parameter);
      }
      else {
        unmatched.add(parameter);
      }
    }
    if (!unmatched.isEmpty()) {
      unmatched.removeAll(functionType.getOptionalParams());
      if (!unmatched.isEmpty()) {
        throw new MatchingException(generateMissingArgErrorMessage(unmatched, 0));
      }
    }

    if (!suppliedArguments.isEmpty()) {
      checkUnmatchedArgs(suppliedArguments);
    }
  }

  private static String generateMissingArgErrorMessage(List<RParameter> parameters, int i) {
    String noDefaultMessage = " missing, with no default";
    if (i == parameters.size() - 1) {
      return "argument \'" + parameters.get(i).getText() + "\' is" + noDefaultMessage;
    }
    StringBuilder stringBuilder = new StringBuilder("arguments ");
    while (i < parameters.size()) {
      stringBuilder.append("\"").append(parameters.get(i).getText()).append("\"").append(", ");
      i++;
    }
    int length = stringBuilder.length();
    return stringBuilder.delete(length - 2, length - 1).append("are").append(noDefaultMessage).toString();
  }

  private static List<RParameter> getMatches(String name, List<RParameter> parameters, boolean usePartial) {
    List<RParameter> matches = new ArrayList<RParameter>();
    for (RParameter param : parameters) {
      if (usePartial && param.getText().equals("...")) {
        return matches;
      }
      String paramName = param.getName();
      if (paramName != null) {
        if (usePartial) {
          if (paramName.startsWith(name)) {
            matches.add(param);
          }
        }
        else {
          if (paramName.equals(name)) {
            matches.add(param);
          }
        }
      }
    }
    return matches;
  }

  private static List<RExpression> getNamedArguments(List<RExpression> arguments) {
    List<RExpression> namedArgs = new ArrayList<RExpression>();
    for (RExpression arg : arguments) {
      if (arg instanceof RAssignmentStatement && arg.getName() != null) {
        namedArgs.add(arg);
      }
    }
    return namedArgs;
  }

  private static void matchParams(List<RParameter> parameters, List<RExpression> arguments,
                                  boolean usePartialMatching,
                                  Map<RExpression, RParameter> matchedParams) throws MatchingException {
    List<RExpression> namedArguments = getNamedArguments(arguments);
    for (RExpression namedArg : namedArguments) {
      String name = namedArg.getName();
      List<RParameter> matches = getMatches(name, parameters, usePartialMatching);
      if (matches.size() > 1) {
        throw new MatchingException("formal argument " + name + " matched by multiply actual arguments");
      }
      if (matches.size() == 1) {
        matchedParams.put(namedArg, matches.get(0));
      }
    }

    for (Map.Entry<RExpression, RParameter> entry : matchedParams.entrySet()) {
      arguments.remove(entry.getKey());
      parameters.remove(entry.getValue());
    }
  }

  private static void checkUnmatchedArgs(List<RExpression> arguments) throws MatchingException {
    int size = arguments.size();
    if (size == 1) {
      throw new MatchingException("unused argument " + arguments.get(0).getText());
    }
    if (size > 0) {
      StringBuilder errorMessage = new StringBuilder("unused arguments: ");
      for (RExpression expression : arguments) {
        errorMessage.append(expression.getText()).append(", ");
      }
      int lastComma = errorMessage.lastIndexOf(",");
      throw new MatchingException(errorMessage.delete(lastComma, lastComma + 1).toString());
    }
  }
}
